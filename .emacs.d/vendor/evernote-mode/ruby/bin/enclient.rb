#! /System/Library/Frameworks/Ruby.framework/Versions/1.8/usr/bin/ruby -sWKu
# -*- coding: utf-8 -*-

#
#  Copyright 2011 Yusuke KAWAKAMI, Akihiro ARISAWA
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#

begin # use rubygems if possible
  require "rubygems"
rescue LoadError
end

require "cgi"
require "singleton"
require "base64"
require "thread"
require "cgi"
require "kconv"
require "fileutils"
require "gdbm"
require "logger"
require "digest/md5"
#require "benchmark"

require "thrift/types"
require "thrift/struct"
require "thrift/protocol/base_protocol"
require "thrift/protocol/binary_protocol"
require "thrift/transport/base_transport"
require "thrift/transport/http_client_transport"
require "Evernote/EDAM/user_store"
require "Evernote/EDAM/user_store_constants"
require "Evernote/EDAM/note_store"
require "Evernote/EDAM/limits_constants"


module EnClient
  module Serializable
    def serialize
      fields = serialized_fields.map do |varsym, vartype|
        varval = send varsym
        if varval
          case vartype
          when :field_type_int, :field_type_bool, :field_type_string, :field_type_timestamp
            "#{varsym}=#{varval}"
          when :field_type_string_array
            "#{varsym}=#{varval.join "|"}"
          when :field_type_base64
            "#{varsym}=#{Formatter.encode_base64 varval}"
          when :field_type_base64_array
            b64list = Formatter.encode_base64_list varval
            "#{varsym}=#{b64list.join "|"}"
          else
            raise IllegalStateException.new("illegal field type #{ftype}")
          end
        end
      end
      fields.delete nil
      fields.join ","
    end

    def deserialize(str)
      fields = str.split ","
      fields.each do |f|
        f =~ /\A([^=]*)=(.*)\z/
        varsym = $1.to_sym
        varval_str = $2
        vartype = serialized_fields[varsym]
        #puts "[#{varsym}], [#{varval_str}], [#{vartype}]"

        varval =
          if varval_str
            case vartype
            when :field_type_int, :field_type_timestamp
              varval_str.to_i
            when :field_type_bool
              if varval_str == "true"
                true
              elsif varval_str == "false"
                false
              else
                raise IllegalStateException.new("illegal field value of boolean #{varval_str}")
              end
            when :field_type_string
              varval_str
            when :field_type_string_array
              varval_str.split "|"
            when :field_type_base64
              Formatter.decode_base64 varval_str
            when :field_type_base64_array
              Formatter.decode_base64_list varval_str.split("|")
            else
              raise IllegalStateException.new("illegal field type #{vartype} for #{varsym}")
            end
          else
            nil
          end
        send (varsym.to_s + "="), varval
      end
    end

    def to_sexp
      str = "("
      class_name = Formatter.remove_package_names self.class.name
      str += "(class . #{class_name})"
      serialized_fields.each do |varsym, vartype|
        varval = send varsym
        if varval
          case vartype
          when :field_type_int
            str += "(#{varsym} . #{varval})"
          when :field_type_timestamp
            str += %|(#{varsym} . "#{Time.at varval/1000}")|
          when :field_type_bool
            if varval == true
              str += "(#{varsym} . t)"
            elsif varval_str == false
              str += "(#{varsym} . nil)"
            else
              raise IllegalStateException.new("illegal field value of boolean #{varval}")
            end
          when :field_type_string
            str += %|(#{varsym} . "#{varval}")|
          when :field_type_string_array
            str += "(#{varsym} . ("
            varval.each do |elem|
              str += %|"#{elem}"|
                str += " "
            end
            str += "))"
          when :field_type_base64
            #str += %|(#{varsym} . "#{Formatter.encode_base64 varval}")|
            str += %|(#{varsym} . "#{Formatter.sexp_string_escape varval}")|
          when :field_type_base64_array
            str += "(#{varsym} . ("
            varval.each do |elem|
              #str += %|"#{Formatter.encode_base64 elem}"|
              # str += " "
              str += %|"#{Formatter.sexp_string_escape elem}"|
                str += " "
            end
            str += "))"
          else
            raise IllegalStateException.new("illegal field type #{vartype}")
          end
        else
          str += "(#{varsym} . nil)"
        end
      end
      str += ")"
    end
  end
end


module Evernote
  module EDAM
    module Type

      class Notebook
        include ::EnClient::Serializable

        def serialized_fields
          { :guid => :field_type_string,
            :name => :field_type_base64,
            :updateSequenceNum => :field_type_int,
            :defaultNotebook => :field_type_bool,
            :serviceCreated => :field_type_timestamp,
            :serviceUpdated => :field_type_timestamp }
        end
      end


      class Note
        include EnClient::Serializable

        attr_accessor :editMode, :contentFile

        def serialized_fields
          { :guid => :field_type_string,
            :title => :field_type_base64,
            :created => :field_type_timestamp,
            :updated => :field_type_timestamp,
            :updateSequenceNum => :field_type_int,
            :notebookGuid => :field_type_string,
            :tagGuids => :field_type_string_array,
            :tagNames => :field_type_base64_array,
            :editMode => :field_type_string,
            :contentFile => :field_type_base64 }
        end
      end


      class Tag
        include EnClient::Serializable

        def serialized_fields
          { :guid => :field_type_string,
            :name => :field_type_base64,
            :parentGuid => :field_type_string,
            :updateSequenceNum => :field_type_int }
        end
      end


      class SavedSearch
        include EnClient::Serializable

        def serialized_fields
          { :guid => :field_type_string,
            :name => :field_type_base64,
            :query => :field_type_base64,
            :format => :field_type_int,
            :updateSequenceNum => :field_type_int }
        end
      end

    end
  end
end


#
# main module
#
module EnClient
  APPLICATION_NAME_TEXT  = %|emacs-enclient {:version => 0.41, :editmode => "TEXT"}|
  APPLICATION_NAME_XHTML = %|emacs-enclient {:version => 0.41, :editmode => "XHTML"}|

  #EVERNOTE_HOST       = "sandbox.evernote.com"
  EVERNOTE_HOST       = "www.evernote.com"
  USER_STORE_URL      = "https://#{EVERNOTE_HOST}/edam/user"
  NOTE_STORE_URL_BASE = "https://#{EVERNOTE_HOST}/edam/note/"

  # error code
  ERROR_CODE_OK = 0
  ERROR_CODE_NOT_FOUND = 100
  ERROR_CODE_UNEXPECTED = 101
  ERROR_CODE_NOT_AUTHED = 102
  ERROR_CODE_TIMEOUT    = 103

  LOG = Logger.new File.expand_path("~/.evernote-mode.log"), 3
  #LOG = Logger.new $stdout
  LOG.level = Logger::WARN


  class NotAuthedException < StandardError; end
  class NotFoundException < StandardError; end
  class IllegalArgumentException < StandardError; end
  class IllegalStateException < StandardError; end


  class HTTPWithProxyClientTransport < Thrift::BaseTransport
    def initialize(url, proxy_addr = nil, proxy_port = nil)
      @url = URI url
      @headers = {'Content-Type' => 'application/x-thrift'}
      @outbuf = ""
      @proxy_addr = proxy_addr
      @proxy_port = proxy_port
    end

    def open?; true end
    def read(sz); @inbuf.read sz end
    def write(buf); @outbuf << buf end

    def add_headers(headers)
      @headers = @headers.merge(headers)
    end

    def flush
      if @proxy_addr && @proxy_port
        http = Net::HTTP::Proxy(@proxy_addr, @proxy_port).new @url.host, @url.port
      else
        http = Net::HTTP.new @url.host, @url.port
      end
      http.use_ssl = @url.scheme == "https"
      #http.verify_mode = OpenSSL::SSL::VERIFY_PEER
      #http.verify_depth = 5
      http.verify_mode = OpenSSL::SSL::VERIFY_NONE
      resp, data = http.post(@url.request_uri, @outbuf, @headers)
      @inbuf = StringIO.new data
      @outbuf = ""
    end
  end


  class TaskQueue
    def initialize
      @cond = ConditionVariable.new
      @mutex = Mutex.new
      @queue = []
    end

    def push(task)
      @mutex.synchronize{
        @queue.push task
        @cond.signal
      }
    end

    def push_to_front(task)
      @mutex.synchronize{
        @queue.unshift task
        @cond.signal
      }
    end

    def pop
      @mutex.synchronize{
        while @queue.size == 0
          @cond.wait(@mutex)
        end
        task = @queue.shift
      }
    end
  end


  class Command
    attr_accessor :command_id, :sm, :dm, :tm, :shell

    def self.create_from_hash(hash)
      unless hash.has_key? :class
        raise IllegalArgumentException.new("key \"class\" is not found")
      end

      class_name = hash[:class]
      command = get_command class_name
      unless command
        raise IllegalArgumentException.new("command #{class_name} is not found")
      end

      hash.each do |key, value|
        next if key == :class
        setter_name = key.to_s + "="
        meth = command.send setter_name, value
      end

      command
    end

    def exec
      exec_impl
    rescue
      reply = ErrorReply.new
      reply.command_id = @command_id
      ErrorUtils.set_reply_error $!, reply
      LOG.warn reply.message
      LOG.warn $!.backtrace
      @shell.reply self, reply
    end

    private

    #
    # Utilities for subclasses
    #

    def server_task(ordered = false, &block)
      task = Task.new do
        begin
          yield
        rescue
          if $!.is_a? SystemCallError
            # workaround for corruption of note_store after timed out
            @sm.fix_note_store
          end
          reply = ErrorReply.new
          reply.command_id = @command_id
          ErrorUtils.set_reply_error $!, reply
          LOG.warn reply.message
          LOG.warn $!.backtrace
          @shell.reply self, reply
        end
      end
      @tm.put task, !ordered
    end

    def check_auth
      @sm.auth_token # check authentication
    end

    #
    # Private helpers
    #

    def self.get_command(name)
      all_commands =
        [AuthCommand,
         ListNoteCommand,
         ListNotebookCommand,
         ListTagCommand,
         ListSearchCommand,
         SearchNoteCommand,
         GetNoteCommand,
         CreateNoteCommand,
         UpdateNoteCommand,
         DeleteNoteCommand,
         CreateNotebookCommand,
         UpdateNotebookCommand,
         CreateTagCommand,
         UpdateTagCommand,
         CreateSearchCommand,
         UpdateSearchCommand]

      command_class = all_commands.find do |elem|
        Formatter.remove_package_names(elem.name) == name
      end
      if command_class
        command_class.new
      end
    end
  end


  class AuthCommand < Command
    attr_accessor :user, :passwd

    def exec_impl
      Formatter.to_ascii @user, @passwd

      server_task do
        sm.authenticate @user, @passwd
        LOG.info "Auth successed: auth_token = '#{sm.auth_token}', shared_id = '#{sm.shared_id}'"
        tm.put SyncTask.new(sm, dm, tm)
        server_task true do # defer reply until first sync will be done.
          shell.reply self, AuthReply.new
        end
      end
    end
  end


  module FormatNoteOperation
    private

    NOTE_DEFAULT_HEADER = %|<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd"><en-note>|
    NOTE_DEFAULT_FOOTER = %|</en-note>|

    def set_attribute_and_format_content!(note)
      note.attributes = Evernote::EDAM::Type::NoteAttributes.new
      if note.editMode == "TEXT"
        note.content = to_xhtml note.content if note.content
        note.attributes.sourceApplication = APPLICATION_NAME_TEXT
      elsif note.editMode == "XHTML"
        note.attributes.sourceApplication = APPLICATION_NAME_XHTML
      end
    end

    def to_xhtml(content)
      content = CGI.escapeHTML content
      content.gsub! %r{ }, %{&nbsp;}
        content.gsub! %r{(?:\r\n)|\n|\r}, %|<br clear="none"/>|
        content = NOTE_DEFAULT_HEADER + content + NOTE_DEFAULT_FOOTER
    end
  end


  class CreateNoteCommand < Command
    attr_accessor :title, :notebook_guid, :tag_names, :edit_mode, :content

    include FormatNoteOperation

    def exec_impl
      Formatter.to_ascii @title, @content, *@tag_names

      note = Evernote::EDAM::Type::Note.new
      note.title = @title
      note.notebookGuid = @notebook_guid
      note.tagNames = @tag_names
      note.editMode = @edit_mode
      note.content = @content
      set_attribute_and_format_content! note

      server_task do
        result_note = sm.note_store.createNote sm.auth_token, note
        result_note.editMode = @edit_mode
        DBUtils.set_note_and_content dm, result_note, @content

        if result_note.tagGuids
          result_note.tagGuids.each do |guid|
            unless DBUtils.exist_tag_in_cache? dm, guid
              tags = sm.note_store.listTags sm.auth_token
              DBUtils.sync_updated_tags dm, tags
              break
            end
          end
        end

        reply = CreateNoteReply.new
        reply.note = result_note
        shell.reply self, reply
      end
    end
  end


  class UpdateNoteCommand < Command
    attr_accessor :guid, :title, :notebook_guid, :tag_names, :content, :edit_mode

    include FormatNoteOperation

    def exec_impl
      Formatter.to_ascii @title, @notebook_guid, @content, *@tag_names

      old_note = DBUtils.get_note dm, @guid

      note = Evernote::EDAM::Type::Note.new
      note.guid = @guid
      if @title
        note.title = @title
      else
        note.title = old_note.title
      end
      note.notebookGuid = @notebook_guid
      note.tagNames = @tag_names
      if @edit_mode
        note.editMode = @edit_mode
      else
        note.editMode = old_note.editMode
      end
      note.content = @content
      set_attribute_and_format_content! note

      server_task do
        result_note = sm.note_store.updateNote sm.auth_token, note
        result_note.editMode = note.editMode
        DBUtils.set_note_and_content dm, result_note, @content
        reply = UpdateNoteReply.new

        if result_note.tagGuids
          result_note.tagGuids.each do |guid|
            unless DBUtils.exist_tag_in_cache? dm, guid
              tags = sm.note_store.listTags sm.auth_token
              DBUtils.sync_updated_tags dm, tags
              break
            end
          end
        end

        reply.note = result_note
        shell.reply self, reply
      end
    end
  end


  class DeleteNoteCommand < Command
    attr_accessor :guid

    def exec_impl
      server_task do
        usn = sm.note_store.deleteNote sm.auth_token, @guid
        note = DBUtils.get_note dm, @guid
        note.updateSequenceNum = usn
        note.active = false
        DBUtils.set_note_and_content dm, note, nil
        shell.reply self, DeleteNoteReply.new
      end
    end
  end


  class CreateNotebookCommand < Command
    attr_accessor :name, :default_notebook

    def exec_impl
      Formatter.to_ascii @name

      notebook = Evernote::EDAM::Type::Notebook.new
      notebook.name = @name
      notebook.defaultNotebook = @default_notebook

      server_task do
        result_notebook = sm.note_store.createNotebook sm.auth_token, notebook
        DBUtils.set_notebook dm, result_notebook
        reply = CreateNotebookReply.new
        reply.notebook = result_notebook
        shell.reply self, reply
      end
    end
  end


  class UpdateNotebookCommand < Command
    attr_accessor :guid, :name, :default_notebook

    def exec_impl
      Formatter.to_ascii @name

      notebook = Evernote::EDAM::Type::Notebook.new
      notebook.guid = @guid
      notebook.name = @name
      notebook.defaultNotebook = @default_notebook

      server_task do
        usn = sm.note_store.updateNotebook sm.auth_token, notebook
        notebook.updateSequenceNum = usn
        DBUtils.set_notebook dm, notebook
        reply = UpdateNotebookReply.new
        reply.notebook = notebook
        shell.reply self, reply
      end
    end
  end


  class CreateTagCommand < Command
    attr_accessor :name, :parent_guid

    def exec_impl
      Formatter.to_ascii @name

      tag = Evernote::EDAM::Type::Tag.new
      tag.name = @name
      tag.parentGuid = @parent_guid

      server_task do
        result_tag = sm.note_store.createTag sm.auth_token, tag
        DBUtils.set_tag dm, result_tag
        reply = CreateTagReply.new
        reply.tag = result_tag
        shell.reply self, reply
      end
    end
  end


  class UpdateTagCommand < Command
    attr_accessor :guid, :name, :parent_guid

    def exec_impl
      Formatter.to_ascii @name

      tag = Evernote::EDAM::Type::Tag.new
      tag.guid = @guid
      tag.name = @name
      tag.parentGuid = @parent_guid

      server_task do
        usn = sm.note_store.updateTag sm.auth_token, tag
        tag.updateSequenceNum = usn
        DBUtils.set_tag dm, tag
        reply = UpdateTagReply.new
        shell.reply self, reply
      end
    end
  end


  class CreateSearchCommand < Command
    attr_accessor :name, :query

    def exec_impl
      Formatter.to_ascii @name

      search = Evernote::EDAM::Type::SavedSearch.new
      search.name = @name
      search.query = @query

      server_task do
        result_search = sm.note_store.createSearch sm.auth_token, search
        DBUtils.set_search dm, result_search
        reply = CreateSearchReply.new
        reply.search = result_search
        shell.reply self, reply
      end
    end
  end


  class UpdateSearchCommand < Command
    attr_accessor :guid, :name, :query

    def exec_impl
      Formatter.to_ascii @name, @query

      search = Evernote::EDAM::Type::SavedSearch.new
      search.guid = @guid
      search.name = @name
      search.query = @query

      server_task do
        usn = sm.note_store.updateSearch sm.auth_token, search
        search.updateSequenceNum = usn
        DBUtils.set_search dm, search
        reply = UpdateSearchReply.new
        shell.reply self, reply
      end
    end
  end


  class SearchNoteCommand < Command
    attr_accessor :query

    def exec_impl
      Formatter.to_ascii @query

      filter = Evernote::EDAM::NoteStore::NoteFilter.new
      filter.order = Evernote::EDAM::Type::NoteSortOrder::UPDATED
      filter.words = @query

      server_task do
        notelist = sm.note_store.findNotes sm.auth_token, filter, 0, Evernote::EDAM::Limits::EDAM_USER_NOTES_MAX
        DBUtils.sync_updated_notes dm, sm, tm, notelist.notes
        reply = SearchNoteReply.new
        reply.notes = notelist.notes
        shell.reply self, reply
      end
    end
  end


  class GetNoteCommand < Command
    attr_accessor :guid

    def exec_impl
      check_auth
      note = DBUtils.get_note dm, @guid
      if note && note.contentFile && (FileTest.readable? note.contentFile)
        reply = GetNoteReply.new
        reply.note = note
        shell.reply self, reply
      else
        server_task do
          note = sm.note_store.getNote sm.auth_token, @guid, true, false, false, false
          note.editMode = Formatter.get_edit_mode note.attributes.sourceApplication
          content = format_content note.content, note.editMode
          DBUtils.set_note_and_content dm, note, content
          reply = GetNoteReply.new
          reply.note = note
          shell.reply self, reply
        end
      end
    end

    private

    def format_content(content, edit_mode)
      result = nil
      content.gsub! %r{(?:\r\n)|\n|\r}, "\n"
      if edit_mode == "TEXT"
        content =~ %r|<en-note[^>]*>(.*)</en-note>|m
        content = $1
        content.gsub! %r{<br.*?/>}m, "\n"
        content.gsub! %r{&nbsp;}m, " "
        result = CGI.unescapeHTML content
      else
        result = content
      end
    end
  end


  class ListNotebookCommand < Command
    @@issued_before = false

    def exec_impl
      check_auth
      if dm.during_full_sync? && !@@issued_before
        get_result_from_server
      else
        get_result_from_local_cache
      end
    end

    private

    def get_result_from_local_cache
      LOG.debug "return notebooks from cache"
      notebooks = DBUtils.get_all_notebooks dm
      notebooks.sort! do |a, b|
        a.name <=> b.name
      end
      reply = ListNotebookReply.new
      reply.notebooks = notebooks
      shell.reply self, reply
    end

    def get_result_from_server
      server_task do
        LOG.debug "return notebooks from server"
        notebooks = sm.note_store.listNotebooks sm.auth_token
        DBUtils.sync_updated_notebooks dm, notebooks
        reply = ListNotebookReply.new
        reply.notebooks = notebooks
        @@issued_before = true
        shell.reply self, reply
      end
    end
  end


  class ListNoteCommand < Command
    attr_accessor :tag_guids, :notebook_guid

    def initialize
      @tag_guids = []
    end

    def exec_impl
      check_auth
      if dm.during_full_sync?
        get_result_from_server
      else
        get_result_from_local_cache
      end
    end

    private

    def get_result_from_local_cache
      LOG.debug "return notes from cache"
      notes = []
      dm.transaction do
        dm.open_note do |db|
          db.each_value do |value|
            n = Evernote::EDAM::Type::Note.new
            n.deserialize value
            if @tag_guids == nil || (n.tagGuids != nil && (@tag_guids - n.tagGuids).empty?)
              if @notebook_guid == nil || @notebook_guid == n.notebookGuid
                notes << n
              end
            end
          end
        end
      end
      notes.sort! do |a, b|
        b.updated <=> a.updated
      end
      reply = ListNoteReply.new
      reply.notes = notes
      shell.reply self, reply
    end

    def get_result_from_server
      LOG.debug "return notes from server"
      server_task do
        filter = Evernote::EDAM::NoteStore::NoteFilter.new
        filter.order = Evernote::EDAM::Type::NoteSortOrder::UPDATED
        filter.tagGuids = @tag_guids
        filter.notebookGuid = @notebook_guid

        notelist = sm.note_store.findNotes(sm.auth_token,
                                           filter,
                                           0,
                                           Evernote::EDAM::Limits::EDAM_USER_NOTES_MAX)
        DBUtils.sync_updated_notes dm, sm, tm, notelist.notes
        reply = ListNoteReply.new
        reply.notes = notelist.notes
        shell.reply self, reply
      end
    end
  end


  class ListTagCommand < Command
    @@issued_before = false

    def exec_impl
      check_auth
      if dm.during_full_sync? && !@@issued_before
        get_result_from_server
      else
        get_result_from_local_cache
      end
    end

    private

    def get_result_from_local_cache
      LOG.debug "return tags from cache"
      tags = DBUtils.get_all_tags dm
      tags.sort! do |a, b|
        a.name <=> b.name
      end
      reply = ListTagReply.new
      reply.tags = tags
      shell.reply self, reply
    end

    def get_result_from_server
      server_task do
        LOG.debug "return tags from server"
        tags = sm.note_store.listTags sm.auth_token
        DBUtils.sync_updated_tags dm, tags
        tags.sort! do |a, b|
          a.name <=> b.name
        end
        reply = ListTagReply.new
        reply.tags = tags
        @@issued_before = true
        shell.reply self, reply
      end
    end
  end


  class ListSearchCommand < Command
    @@issued_before = false

    def exec_impl
      check_auth
      if dm.during_full_sync? && !@@issued_before
        get_result_from_server
      else
        get_result_from_local_cache
      end
    end

    private

    def get_result_from_local_cache
      LOG.debug "return searches from cache"
      searches = DBUtils.get_all_searches dm
      searches.sort! do |a, b|
        a.name <=> b.name
      end
      reply = ListSearchReply.new
      reply.searches = searches
      shell.reply self, reply
    end

    def get_result_from_server
      server_task do
        LOG.debug "return searches from server"
        searches = sm.note_store.listSearches sm.auth_token
        DBUtils.sync_updated_searches dm, searches
        searches.sort! do |a, b|
          a.name <=> b.name
        end
        reply = ListSearchReply.new
        reply.searches = searches
        @@issued_before = true
        shell.reply self, reply
      end
    end
  end


  class Reply
    attr_accessor :command_id
  end


  class ErrorReply < Reply
    attr_accessor :result_code, :message
  end


  class AuthReply < Reply
  end


  class ListNotebookReply < Reply
    attr_accessor :notebooks
  end


  class ListNoteReply < Reply
    attr_accessor :notes
  end


  class ListTagReply < Reply
    attr_accessor :tags
  end


  class ListSearchReply < Reply
    attr_accessor :searches
  end


  class SearchNoteReply < Reply
    attr_accessor :notes
  end


  class GetNoteReply < Reply
    attr_accessor :note
  end


  class CreateNoteReply < Reply
    attr_accessor :note
  end


  class UpdateNoteReply < Reply
    attr_accessor :note
  end


  class DeleteNoteReply < Reply
  end


  class CreateNotebookReply < Reply
    attr_accessor :notebook
  end


  class UpdateNotebookReply < Reply
    attr_accessor :notebook
  end


  class CreateTagReply < Reply
    attr_accessor :tag
  end


  class UpdateTagReply < Reply
  end


  class CreateSearchReply < Reply
    attr_accessor :search
  end


  class UpdateSearchReply < Reply
  end


  class Task
    def initialize(&block)
      @proc = Proc.new
    end

    def exec
      @proc.call
    end
  end


  class SyncTask < Task
    MAX_SYNCED_ENTRY = 100
    #MAX_SYNCED_ENTRY = 1 for test

    def initialize(sm, dm, tm)
      @sm = sm
      @dm = dm
      @tm = tm
    end

    def exec
      note_store = @sm.note_store
      sync_state = note_store.getSyncState @sm.auth_token

      LOG.info "[sync state begin]"
      LOG.info "currentTime    = #{sync_state.currentTime}"
      LOG.info "fullSyncBefore = #{sync_state.fullSyncBefore}"
      LOG.info "updateCount    = #{sync_state.updateCount}"
      LOG.info "[sync state end]"
      LOG.info "expiration     = #{@sm.expiration}"

      @sm.refresh_authentication sync_state.currentTime
      last_sync, usn = DBUtils.get_last_sync_and_usn @dm

      LOG.info "[current state begin]"
      LOG.info "last_sync      = #{last_sync}"
      LOG.info "USN            = #{usn}"
      LOG.info "[current state end]"

      return if sync_state.updateCount == usn

      is_full_sync = false

      if last_sync < sync_state.fullSyncBefore
        @dm.transaction do
          @dm.clear_db
        end
        @dm.set_during_full_sync true
        LOG.debug "begin full sync"
      end

      if @dm.during_full_sync?
        is_full_sync = true
      end

      sync_chunk = note_store.getSyncChunk @sm.auth_token, usn, MAX_SYNCED_ENTRY, is_full_sync
      LOG.debug "sync (#{usn}-#{sync_chunk.chunkHighUSN}) full_sync = #{is_full_sync}"
      sync_db sync_chunk

      if sync_chunk.chunkHighUSN < sync_chunk.updateCount
        @tm.put SyncTask.new(@sm, @dm, @tm)
      else
        @dm.set_during_full_sync false
        LOG.debug "finish full sync"
      end

    rescue
      if $!.is_a? SystemCallError
        # workaround for corruption of note_store after timed out
        @sm.fix_note_store
      end
      message = ErrorUtils.get_message $!
      LOG.warn message
      LOG.warn $!.backtrace
    end

    private

    def sync_db(sync_chunk)
      DBUtils.sync_updated_notebooks @dm, sync_chunk.notebooks if sync_chunk.notebooks
      DBUtils.sync_updated_notes @dm, @sm, @tm, sync_chunk.notes if sync_chunk.notes
      DBUtils.sync_updated_tags @dm, sync_chunk.tags if sync_chunk.tags
      DBUtils.sync_updated_searches @dm, sync_chunk.searches if sync_chunk.searches
      DBUtils.sync_expunged_notebooks @dm, sync_chunk.expungedNotebooks if sync_chunk.expungedNotebooks
      DBUtils.sync_expunged_notes @dm, sync_chunk.expungedNotes, @tm if sync_chunk.expungedNotes
      DBUtils.sync_expunged_tags @dm, sync_chunk.expungedTags if sync_chunk.expungedTags
      DBUtils.sync_expunged_searches @dm, sync_chunk.expungedSearches if sync_chunk.expungedSearches
      DBUtils.set_last_sync_and_usn @dm, sync_chunk.currentTime, sync_chunk.chunkHighUSN
    end
  end


  class SessionManager
    REFRESH_LIMIT_SEC = 300

    def initialize
      @auth_token = nil
      @shared_id  = nil
      @note_store = nil
      @user_store = nil
      @expiration = nil
    end

    def auth_token
      raise NotAuthedException.new("Not authed") unless @auth_token
      @auth_token
    end

    def shared_id
      raise NotAuthedException.new("Not authed") unless @shared_id
      @shared_id
    end

    def note_store
      raise NotAuthedException.new("Not authed") unless @note_store
      @note_store
    end

    def user_store
      raise NotAuthedException.new("Not authed") unless @user_store
      @user_store
    end

    def expiration
      raise NotAuthedException.new("Not authed") unless @expiration
      @expiration
    end

    def authenticate(user, passwd)
      appname = "kawayuu"
      appid = "24b37bd1326624a0"
      @user_store = create_user_store
      auth_result = @user_store.authenticate user, passwd, appname, appid
      @auth_token, @shared_id, @expiration = get_session auth_result
      @note_store = create_note_store @shared_id
    end

    def refresh_authentication(current_time)
      if current_time > @expiration - REFRESH_LIMIT_SEC * 1000
        LOG.info "refresh authentication"
        auth_result = @user_store.refreshAuthentication @auth_token
        @auth_token, dummy, @expiration = get_session auth_result
        @note_store = create_note_store @shared_id
      end
    end

    def fix_note_store
      if @shared_id
        @note_store = create_note_store @shared_id
      else
        @note_store = nil
      end
    end

    private

    def create_user_store
      proxy_host, proxy_port = get_proxy

      if proxy_host
        user_store_transport = HTTPWithProxyClientTransport.new USER_STORE_URL, proxy_host, proxy_port
      else
        user_store_transport = HTTPWithProxyClientTransport.new USER_STORE_URL
      end
      user_store_protocol = Thrift::BinaryProtocol.new user_store_transport
      user_store = Evernote::EDAM::UserStore::UserStore::Client.new user_store_protocol

      version_ok = user_store.checkVersion("Emacs Client",
                                           Evernote::EDAM::UserStore::EDAM_VERSION_MAJOR,
                                           Evernote::EDAM::UserStore::EDAM_VERSION_MINOR)

      unless version_ok
        raise IllegalStateException.new("UserStore version invalid")
      end
      user_store
    end

    def create_note_store(shared_id)
      note_store_url = NOTE_STORE_URL_BASE + shared_id

      proxy_host, proxy_port = get_proxy
      if proxy_host
        note_store_transport = HTTPWithProxyClientTransport.new note_store_url, proxy_host, proxy_port
      else
        note_store_transport = HTTPWithProxyClientTransport.new note_store_url
      end

      note_store_protocol = Thrift::BinaryProtocol.new note_store_transport
      Evernote::EDAM::NoteStore::NoteStore::Client.new note_store_protocol
    end

    def get_session(auth_result)
      auth_token = auth_result.authenticationToken
      shared_id  = auth_result.user.shardId if auth_result.user
      expiration = auth_result.expiration
      [auth_token, shared_id, expiration]
    end

    def get_proxy
      proxy_str = ENV["EN_PROXY"]
      if proxy_str
        proxy_str =~ /((?:\w|\.)+):([0-9]+)/
        [$1, $2]
      else
        nil
      end
    end
  end


  class DBManager
    ENMODE_SYS_DIR      = File.expand_path("~/.evernote-mode") + "/"
    DB_LOCK             = ENMODE_SYS_DIR  + "lock"
    DB_SYNC             = ENMODE_SYS_DIR  + "sync"
    DB_NOTEBOOK         = ENMODE_SYS_DIR  + "notebook"
    DB_NOTE             = ENMODE_SYS_DIR  + "note"
    DB_TAG              = ENMODE_SYS_DIR  + "tag"
    DB_SAVED_SEARCH     = ENMODE_SYS_DIR  + "saved_search"
    CONTENT_DIR         = ENMODE_SYS_DIR  + "contents/"

    DB_SYNC_LAST_SYNC_FIELD = 'last_sync'
    DB_SYNC_USN_FIELD       = 'usn'


    def initialize
      unless FileTest.directory? ENMODE_SYS_DIR
        FileUtils.mkdir ENMODE_SYS_DIR
      end

      unless FileTest.directory? CONTENT_DIR
        FileUtils.mkdir CONTENT_DIR
      end

      @lock_file = open DB_LOCK, 'w'
      @mutex = Mutex.new
      @in_transaction = false
      @is_during_full_sync = false
    end

    def transaction
      @mutex.lock
      @lock_file.flock File::LOCK_EX
      @in_transaction = true
      yield
    ensure
      @in_transaction = false
      @lock_file.flock File::LOCK_UN
      @mutex.unlock
    end

    def clear_db
      raise IllegalStateException.new("not in transaction") unless @in_transaction
      [DB_SYNC, DB_NOTEBOOK, DB_NOTE, DB_TAG, DB_SAVED_SEARCH].each do |file|
        GDBM.open file do |db|
          db.clear
        end
      end
    end

    def open_sync(&block)
      raise IllegalStateException.new("not in transaction") unless @in_transaction
      GDBM.open DB_SYNC, &block
    end

    def open_notebook(&block)
      raise IllegalStateException.new("not in transaction") unless @in_transaction
      GDBM.open DB_NOTEBOOK, &block
    end

    def open_note(&block)
      raise IllegalStateException.new("not in transaction") unless @in_transaction
      GDBM.open DB_NOTE, &block
    end

    def open_tag(&block)
      raise IllegalStateException.new("not in transaction") unless @in_transaction
      GDBM.open DB_TAG, &block
    end

    def open_search(&block)
      raise IllegalStateException.new("not in transaction") unless @in_transaction
      GDBM.open DB_SAVED_SEARCH, &block
    end

    # note content

    def set_note_content(guid, content)
      raise IllegalStateException.new("not in transaction") unless @in_transaction
      file_path = CONTENT_DIR + guid
      open file_path, "w" do |file| # "w" for transform eols to the native ones
        file.write content
      end
      LOG.info "update content at #{file_path}"
      file_path
    end

    def remove_note_content(guid)
      raise IllegalStateException.new("not in transaction") unless @in_transaction
      file_path = CONTENT_DIR + guid
      LOG.info "try to expunge content at #{file_path}"
      FileUtils.rm file_path if FileTest.file? file_path
      LOG.info "expunge content at #{file_path}"
    end

    def during_full_sync?
      result = nil
      @mutex.synchronize do
        result = @is_during_full_sync
      end
      result
    end

    def set_during_full_sync(state)
      LOG.debug "during full sync: #{state}"
      @mutex.synchronize do
        @is_during_full_sync = state
      end
    end

  end


  class DBUtils
    def self.get_last_sync_and_usn(dm)
      last_sync, usn = nil, nil
      dm.transaction do
        dm.open_sync do |db|
          last_sync, usn = db[DBManager::DB_SYNC_LAST_SYNC_FIELD], db[DBManager::DB_SYNC_USN_FIELD]
        end
      end

      if last_sync
        last_sync = last_sync.to_i
      else
        last_sync = 0
      end
      if usn
        usn = usn.to_i
      else
        usn = 0
      end

      [last_sync, usn]
    end

    def self.set_last_sync_and_usn(dm, last_sync, usn)
      dm.transaction do
        dm.open_sync do |db|
          db[DBManager::DB_SYNC_LAST_SYNC_FIELD] = last_sync.to_s
          db[DBManager::DB_SYNC_USN_FIELD] = usn.to_s
        end
      end
    end

    def self.get_all_notebooks(dm)
      notebooks = []
      dm.transaction do
        dm.open_notebook do |db|
          db.each_value do |value|
            nb = Evernote::EDAM::Type::Notebook.new
            nb.deserialize value
            notebooks << nb
          end
        end
      end
      notebooks
    end

    def self.get_note(dm, guid)
      note = Evernote::EDAM::Type::Note.new
      dm.transaction do
        dm.open_note do |db|
          if db.has_key? guid
            note.deserialize db[guid]
          else
            raise NotFoundException.new("Note guid #{guid} is not found")
          end
        end
      end
      note
    end

    def self.set_note_and_content(dm, note, content)
      dm.transaction do
        dm.open_note do |db|
          note.contentFile = dm.set_note_content note.guid, content if content
          db[note.guid] = note.serialize
        end
      end
    end

    def self.get_all_tags(dm)
      tags = []
      dm.transaction do
        dm.open_tag do |db|
          db.each_value do |value|
            t = Evernote::EDAM::Type::Tag.new
            t.deserialize value
            tags << t
          end
        end
      end
      tags
    end

    def self.exist_tag_in_cache?(dm, guid)
      dm.transaction do
        dm.open_tag do |db|
          if db.has_key? guid
            return true
          end
        end
      end
      return false
    end

    def self.set_tag(dm, tag)
      dm.transaction do
        dm.open_tag do |db|
          db[tag.guid] = tag.serialize
        end
      end
    end

    def self.get_all_searches(dm)
      searches = []
      dm.transaction do
        dm.open_search do |db|
          db.each_value do |value|
            s = Evernote::EDAM::Type::SavedSearch.new
            s.deserialize value
            searches << s
          end
        end
      end
      searches
    end

    def self.set_search(dm, search)
      dm.transaction do
        dm.open_search do |db|
          db[search.guid] = search.serialize
        end
      end
    end

    def self.set_notebook(dm, notebook)
      dm.transaction do
        dm.open_notebook do |db|
          if notebook.defaultNotebook
            # unset defaultNotebook of all notebooks
            db.each_value do |value|
              n = Evernote::EDAM::Type::Notebook.new
              n.deserialize value
              if n.guid != notebook.guid
                n.defaultNotebook = false
                db[n.guid] = n.serialize
              end
            end
          end
          db[notebook.guid] = notebook.serialize
        end
      end
    end

    def self.sync_updated_notebooks(dm, notebooks)
      dm.transaction do
        dm.open_notebook do |db|
          notebooks.each do |new_notebook|
            if db.has_key? new_notebook.guid
              current_notebook = Evernote::EDAM::Type::Notebook.new
              current_notebook.deserialize db[new_notebook.guid]
              if current_notebook.updateSequenceNum < new_notebook.updateSequenceNum
                db[new_notebook.guid] = new_notebook.serialize
              end
            else
              db[new_notebook.guid] = new_notebook.serialize
            end
          end
        end
      end
    end

    def self.sync_updated_notes(dm, sm, tm, notes)
      dm.transaction do
        dm.open_note do |db|
          notes.each do |new_note|
            # this method set editMode.
            new_note.editMode = Formatter.get_edit_mode new_note.attributes.sourceApplication
            if db.has_key? new_note.guid
              current_note = Evernote::EDAM::Type::Note.new
              current_note.deserialize db[new_note.guid]
              if current_note.updateSequenceNum < new_note.updateSequenceNum
                dm.remove_note_content new_note.guid # remove content cache if updated
                db[new_note.guid] = new_note.serialize # update note info
              end
            else
              db[new_note.guid] = new_note.serialize
            end
          end
        end
      end
    end

    def self.sync_updated_tags(dm, tags)
      dm.transaction do
        dm.open_tag do |db|
          tags.each do |new_tag|
            if db.has_key? new_tag.guid
              current_tag = Evernote::EDAM::Type::Tag.new
              current_tag.deserialize db[new_tag.guid]
              if current_tag.updateSequenceNum < new_tag.updateSequenceNum
                db[new_tag.guid] = new_tag.serialize
              end
            else
              db[new_tag.guid] = new_tag.serialize
            end
          end
        end
      end
    end

    def self.sync_updated_searches(dm, searches)
      dm.transaction do
        dm.open_search do |db|
          searches.each do |new_search|
            if db.has_key? new_search.guid
              current_search = Evernote::EDAM::Type::SavedSearch.new
              current_search.deserialize db[new_search.guid]
              if current_search.updateSequenceNum < new_search.updateSequenceNum
                db[new_search.guid] = new_search.serialize
              end
            else
              db[new_search.guid] = new_search.serialize
            end
          end
        end
      end
    end

    def self.sync_expunged_notebooks(dm, guids)
      dm.transaction do
        dm.open_notebook do |db|
          guids.each do |guid|
            db.delete guid
          end
        end
      end
    end

    def self.sync_expunged_notes(dm, guids, tm = nil)
      dm.transaction do
        dm.open_note do |db|
          guids.each do |guid|
            dm.remove_note_content guid # remove content cache if updated
            db.delete guid
          end
        end
      end
    end

    def self.sync_expunged_tags(dm, guids)
      dm.transaction do
        dm.open_tag do |db|
          guids.each do |guid|
            db.delete guid
          end
        end
      end
    end

    def self.sync_expunged_searches(dm, guids)
      dm.transaction do
        dm.open_search do |db|
          guids.each do |guid|
            db.delete guid
          end
        end
      end
    end
  end


  class TaskManager
    def initialize
      @task_queue = TaskQueue.new
    end

    def put(task, high_prio = false)
      if high_prio
        @task_queue.push_to_front task
      else
        @task_queue.push task
      end
    end

    def run
      Thread.start do
        LOG.debug "start task manager"
        while true
          task = @task_queue.pop
          LOG.debug "exec #{task}"
          begin
            task.exec
          rescue Exception
            message = ErrorUtils.get_message $!
            LOG.error message
            LOG.error $!.backtrace
          end
        end
      end
    end
  end


  class Timer
    def self.repeat_every(interval)
      while true
        spent_time = time_block { yield }
        sleep(interval - spent_time) if spent_time < interval
      end
    end

    private

    def self.time_block
      start_time = Time.now
      yield
      Time.now - start_time
    end
  end


  class Formatter
    def self.get_edit_mode(src_app)
      result = "XHTML"
      if src_app != nil
        src_app = src_app.strip
        if src_app =~ /\Aemacs-enclient\s*\{.*:editmode\s*=>\s*"(.*)"[^\}]*\}\z/
          result = $1
          unless result == "TEXT" || result == "XHTML"
            result = "TEXT"
          end
        end
      end
      result
    end

    def self.encode_base64(str)
      b64str = Base64.encode64 str
      b64str.delete "\n\r"
    end

    def self.encode_base64_list(str_list)
      str_list.map do |elem|
        encode_base64 elem
      end
    end

    def self.decode_base64(b64str)
      Base64.decode64 b64str
    end

    def self.decode_base64_list(b64list)
      b64list.map do |elem|
        decode_base64 elem
      end
    end

    def self.obj_to_sexp(obj)
      case obj
      when Integer
        return "#{obj}"
      when TrueClass
        return "t"
      when FalseClass, NilClass
        return "nil"
      when String
        return %|"#{obj}"| # must be base64ed
      when Time
        return %|"#{obj}"|
      when Array
        str = "("
        obj.each do |elem|
          str += obj_to_sexp elem
          str += " "
        end
        str += ")"
      when Serializable
        obj.to_sexp
      else
        str = "("
        class_name = Formatter.remove_package_names obj.class.name
        str += "(class . #{class_name})"
        obj.instance_variables.each do |varsym|
          varname = varsym.to_s[1 .. -1] # remove "@"
          str += "(#{varname} . #{obj_to_sexp obj.instance_variable_get(varsym)})"
        end
        str += ")"
      end
    end

    def self.sexp_string_escape(str)
      str.gsub(/\\/,'\&\&').gsub(/"/, '\\"')
    end

    def self.remove_package_names(full_class_name)
      full_class_name.split("::")[-1]
    end

    IS_FORCE_ENCODING_SUPPORTED = "".respond_to? :force_encoding
    def self.to_ascii(*rest)
      if IS_FORCE_ENCODING_SUPPORTED
        rest.each do |elem|
          elem.force_encoding Encoding::ASCII_8BIT if elem
        end
      end
    end
  end


  class ErrorUtils # from here,  rename the appropriate name.
    def self.set_reply_error(ex, reply)
      case ex
      when Evernote::EDAM::Error::EDAMUserException
        errorText = Evernote::EDAM::Error::EDAMErrorCode::VALUE_MAP[ex.errorCode]
        reply.result_code = ex.errorCode
        reply.message = "#{ex.class.name} (parameter: #{ex.parameter} errorCode: #{errorText})"
      when Evernote::EDAM::Error::EDAMSystemException
        errorText = Evernote::EDAM::Error::EDAMErrorCode::VALUE_MAP[ex.errorCode]
        reply.result_code = ex.errorCode
        reply.message = "#{ex.class.name} (message: #{ex.message} errorCode: #{errorText})"
      when Evernote::EDAM::Error::EDAMNotFoundException
        reply.result_code = ERROR_CODE_NOT_FOUND
        reply.message = "#{ex.class.name} (identifier: #{ex.identifier} key: #{ex.key})"
      when NotAuthedException
        reply.result_code = ERROR_CODE_NOT_AUTHED
        reply.message = "Not authenticated"
      else
        reply.result_code = ERROR_CODE_UNEXPECTED
        reply.message = ex.message
      end
      reply.message = Formatter.sexp_string_escape reply.message
    end

    def self.get_message(ex)
      case ex
      when Evernote::EDAM::Error::EDAMUserException
        errorText = Evernote::EDAM::Error::EDAMErrorCode::VALUE_MAP[ex.errorCode]
        "#{ex.class.name} (parameter: #{ex.parameter} errorCode: #{errorText})"
      when Evernote::EDAM::Error::EDAMSystemException
        errorText = Evernote::EDAM::Error::EDAMErrorCode::VALUE_MAP[ex.errorCode]
        "#{ex.class.name} (message: #{ex.message} errorCode: #{errorText})"
      when Evernote::EDAM::Error::EDAMNotFoundException
        "#{ex.class.name} (identifier: #{ex.identifier} key: #{ex.key})"
      when Errno::ETIMEDOUT
        "Connection timed out"
      else
        ex.message
      end
    end
  end


  class Shell
    AUTO_SYNC_INTERVAL = 60

    def run
      sm = SessionManager.new
      dm = DBManager.new
      tm = TaskManager.new
      tm.run

      Thread.start do
        Timer.repeat_every AUTO_SYNC_INTERVAL do
          tm.put SyncTask.new(sm, dm, tm)
        end
      end

      begin
        #if $stdin.respond_to? :set_encoding
        #  LOG.debug "get stdin encoding #{$stdin.external_encoding}, #{$stdin.internal_encoding}"
        #  $stdin.set_encoding "UTF-8", "UTF-8"
        #  $stdout.set_encoding "UTF-8", "UTF-8"
        #  LOG.debug "get stdin encoding #{$stdin.external_encoding}, #{$stdin.internal_encoding}"
        #end
        while true
          begin
            line = $stdin.gets "\000"
            hash = eval line
            LOG.debug "<#{hash[:class]}>"
            command = Command.create_from_hash hash
            command.sm = sm
            command.dm = dm
            command.tm = tm
            command.shell = self
            command.exec
          rescue SyntaxError
            LOG.error $!.backtrace
          rescue
            LOG.error $!.backtrace
          end
        end
      rescue Interrupt
        LOG.debug "Interrupted"
      end
    end

    def reply(command, reply)
      reply.command_id = command.command_id
      $stdout.write Formatter.obj_to_sexp(reply)
      $stdout.flush
    end

  end

end # module EnClient


if __FILE__ == $0
  EnClient::Shell.new.run
end

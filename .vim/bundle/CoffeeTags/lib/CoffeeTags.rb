# encoding: utf-8
require "CoffeeTags/version"
require "CoffeeTags/parser"
require "CoffeeTags/formatter"
require "pathname"
require 'optparse'
require 'erb'

class Object
  def blank?
    if self.respond_to? :"empty?"
      self.nil? or self.empty?
    else
      self.nil?
    end
  end
end

module Coffeetags
  AUTHOR = "Łukasz Korecki	/lukasz@coffeesounds.com/"
  NAME = "CoffeeTags"
  URL = "https://github.com/lukaszkorecki/CoffeeTags"

  class Utils

    def self.option_parser args
      args << '-h' if args.empty?
      options = {}
      optparse = OptionParser.new do |opts|
        opts.banner  = (<<-BAN
          ---------------------------------------------------------------------
          #{NAME} #{Coffeetags::VERSION}
          ---------------------------------------------------------------------
          by #{AUTHOR} ( #{URL} )
          Usage:
          coffeetags [OPTIONS] <list of files>

          CoffeeTags + TagBar + Vim ---> https://gist.github.com/1935512
          ---------------------------------------------------------------------
          BAN
                       ).gsub(/^\s*/,'')


        opts.on('--include-vars', "Include variables in generated tags") do |o|
          options[:include_vars] = true
        end

        opts.on('-f', '--file FILE', 'Write tags to FILE (use - for std out)') do |o|
          options[:output] = o unless o == '-'
        end

        opts.on('-a', '--append', 'Append tags to existing tags file') do |o|
          options[:append] = true
        end

        opts.on('-R', '--recursive', 'Process current directory recursively') do |o|
          options[:recur] = true
        end

        opts.on('--tag-relative', 'Should paths be relative to location of tag file?') do |o|
          options[:tag_relative] = true
        end

        opts.on('-v', '--version', 'Current version') do
          puts Coffeetags::VERSION
          exit
        end

        opts.on('--list-kinds', 'Lists the tag kinds') do
          Coffeetags::Formatter.kinds().map { |k, v| puts "#{k}  #{v}" }
          options[:exit] = true
        end

        opts.on('-h','--help','HELP') do
          puts opts
          options[:exit] = true
        end

      end

      optparse.parse! args

      options[:files]  = args.to_a
      options[:files] += Dir['./**/*.coffee', './**/Cakefile'] if options[:recur]

      options
    end


    def self.run options
      exit if options[:exit]

      output = options[:output]
      include_vars = options[:include_vars]
      files = options[:files]

      files = [files] if files.is_a? String
      files = files.reject { |f| f =~ /^-/}

      lines = setup_tag_lines output, files, options[:append]

      __out = if output.nil?
                STDOUT
              else
                File.open output, 'w'
              end

      __out  << Coffeetags::Formatter.header

      files.each do |file|
        sc = File.read file
        parser = Coffeetags::Parser.new sc, include_vars
        parser.execute!

        tag_file_path = file_path file, output, options[:tag_relative]
        formatter = Coffeetags::Formatter.new tag_file_path, parser.tree

        formatter.parse_tree

        lines.concat(formatter.lines)
      end

      lines.sort!
      __out << lines.map { |l| "#{l}\n"}.join('')

      __out.close if __out.respond_to? :close

      __out.join("\n") if __out.is_a? Array
    end

    def self.setup_tag_lines output, files, append
      return [] unless append
      return [] if output.nil?
      return [] unless File.exists? output

      files = [] if files.nil?
      absolute_files = files.map {|f| Pathname.new(f).expand_path }

      output_dir = Pathname.new(output).dirname

      File.readlines(output).
        map {|l| l.strip}.
        reject {|l| l =~ /^!_/ }.
        reject {|l|
          raw_file_path = l.split("\t")[1]
          tag_file = Pathname.new(raw_file_path)

          tag_file = output_dir + tag_file if raw_file_path =~ /^\./

          absolute_files.include? tag_file.expand_path
        }
    end

    def self.file_path file, output, tag_relative
      return file if output.nil?
      return file unless tag_relative

      output_path = Pathname.new(output).expand_path
      file_path = Pathname.new(file).expand_path

      file_path.relative_path_from(output_path.dirname).to_s
    end

  end
end

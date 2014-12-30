module Coffeetags
  class Parser
    attr_reader :tree
    # Creates a new parser
    #
    # @param [String] source source of the CoffeeScript file
    # @param [Bool] include_vars include objects in generated tree (default false)
    # @return [Coffeetags::Parser]
    def initialize source, include_vars = false
      @include_vars = include_vars
      @source = source

      @fake_parent = 'window'

      # tree maps the ... tree :-)
      @tree = []

      # regexes
      @block = /^\s*(if|unless|switch|loop|do)/
      @class_regex = /\s*class\s*(?:@)?([\w\.]*)/
      @proto_meths = /^\s*([A-Za-z]*)::([@a-zA-Z0-9_]*)/
      @var_regex = /([@a-zA-Z0-9_]*)\s*[:=]\s*$/
      @token_regex = /([@a-zA-Z0-9_]*)\s*[:=]/
      @iterator_regex = /^\s*for\s*([a-zA-Z0-9_]*)\s*/
      @comment_regex = /^\s*#/
      @start_block_comment_regex = /^\s*###/
      @end_block_comment_regex = /^.*###/
      @oneline_block_comment_regex = /^\s*###.*###/
      @comment_lines = mark_commented_lines
    end

    # Mark line numbers as commented out
    # either by single line comment (#)
    # or block comments (###~###)
    def mark_commented_lines
      [].tap do |reg|
        in_block_comment = false
        line_no = 0
        start_block = 0
        end_block = 0
        @source.each_line do |line|
          line_no = line_no+1

          start_block = line_no if !in_block_comment and line =~ @start_block_comment_regex
          end_block = line_no if start_block < line_no and line =~ @end_block_comment_regex
          end_block = line_no if line =~ @oneline_block_comment_regex

          in_block_comment = end_block < start_block

          reg << line_no if in_block_comment or end_block == line_no or line =~ @comment_regex
        end
      end
    end

    # Detect current line level based on indentation
    # very useful in parsing, since CoffeeScript's syntax
    # depends on whitespace
    # @param [String] line currently parsed line
    # @return  [Integer]
    def line_level line
      line.match(/^[ \t]*/)[0].gsub("\t", " ").split('').length
    end

    # Generate current scope path, for example:
    #   e  ->
    #     f ->
    #       z ->
    # Scope path for function z would be:
    # window.e.f
    # @param [Hash, nil] _el element of a prase tree (last one for given tree is used by default)
    # @param [Array, nil] _tree parse tree (or currently built)
    # @returns [String] string representation of scope for given element
    def scope_path _el = nil, _tree = nil
      bf = []
      tree = (_tree || @tree)
      element = (_el || tree.last)
      idx = tree.index(element) || -1

      current_level = element[:level]
      tree[0..idx].reverse.each_with_index do |item, index|
        # uhmmmmmm
        if item[:level] != current_level and item[:level] < current_level and item[:line] !~  @block
          bf << item[:name] unless item[:kind] == 'b'
          current_level = item[:level]
        end
      end
      bf.uniq.reverse.join('.')
    end

    # Helper function for generating parse tree elements for given
    # line and regular expression
    #
    # @param [String] line source line currently being parsed
    # @param [RegExp] regex regular expression for matching a syntax element
    # @param [Integer] level current indentation/line level
    # @param [Hash, {}] additional_fields additional fields which need to be added to generated element
    # @returns [Hash,nil] returns a parse tree element consiting of:
    #   :name of the element
    #   indentation :level of the element
    def item_for_regex line,  regex, level, additional_fields={}
      if item = line.match(regex)
        entry_for_item = {
          :level => level
        }
        if item.length > 2 # proto method
          entry_for_item[:parent] = item[1]
          entry_for_item[:name] = item[2]
        else
          entry_for_item[:name] = item[1]
        end
        entry_for_item.merge(additional_fields)
      end
    end

    # Parse the source and create a tags tree
    # @note this method mutates @tree instance variable of Coffeetags::Parser instance
    # @returns self it can be chained
    def execute!
      line_n = 0
      level = 0
      @source.each_line do |line|
        line_n += 1
        line.chomp!
        # indentify scopes
        level = line_level line

        # ignore comments!
        next if @comment_lines.include? line_n

        [
          [@class_regex, 'c'],
          [@proto_meths, 'p'],
          [@var_regex, 'v'],
          [@block, 'b']
        ].each do |regex, kind|
          mt = item_for_regex line, regex, level, :source => line, :line => line_n, :kind => kind
          @tree << mt unless mt.nil?
        end


        # instance variable or iterator (for/in)?
        token = line.match(@token_regex )
        token ||=  line.match(@iterator_regex)

        # we have found something!
        if not token.nil?
          o = {
            :name => token[1],
            :level => level,
            :parent => '',
            :source => line,
            :line => line_n
          }

          # remove edge cases for now
          # - if a line containes a line like:  element.getElement('type=[checkbox]').lol()
          is_in_string = line =~ /.*['"].*#{token[1]}.*=.*["'].*/

          # - scope access and comparison in if x == 'lol'
          is_in_comparison = line =~ /::|==/

          # - objects with blank parent (parser bug?)
          has_blank_parent = o[:parent] =~ /\.$/

          # - multiple consecutive assignments
          is_previous_not_the_same = !(@tree.last and @tree.last[:name] == o[:name] and  @tree.last[:level] == o[:level])

          if is_in_string.nil? and is_in_comparison.nil? and has_blank_parent.nil? and is_previous_not_the_same
            o[:kind]   =  line =~ /[:=]{1}.*[-=]\>/ ? 'f' : 'o'
            o[:parent] =  scope_path o
            o[:parent] = @fake_parent if o[:parent].empty?

            @tree << o if o[:kind] == 'f'
            @tree << o if o[:kind] == 'o' and @include_vars
          end
        end
        # get rid of duplicate entries
        @tree.uniq!
      end
      self # chain!
    end
  end
end

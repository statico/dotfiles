require 'spec_helper'
describe 'CoffeeTags::Parser' do
  before :all do
    @campfire_class = File.read File.expand_path('./spec/fixtures/campfire.coffee')
    @class_with_dot = File.read File.expand_path('./spec/fixtures/class_with_dot.coffee')
    @class_with_at = File.read File.expand_path('./spec/fixtures/class_with_at.coffee')
    @test_file = File.read File.expand_path('./spec/fixtures/test.coffee')

    @cf_tree = YAML::load_file File.expand_path('./spec/fixtures/tree.yaml')
    @test_tree = YAML::load_file File.expand_path('./spec/fixtures/test_tree.yaml')

  end

  it "should work" do
    lambda { Coffeetags::Parser.new "\n" }.should_not raise_exception
  end


  context 'detect item level' do
    before :each do
      @parser = Coffeetags::Parser.new ''
    end

    it 'gets level from a string with no indent' do
      @parser.line_level("zooo").should == 0
    end

    it "gets level from spaces" do
      @parser.line_level("    def lol").should == 4
    end

    it "gets level from tabs" do
      @parser.line_level("\t\t\tdef lol").should == 3
    end
  end


  context "Creating scope path" do
    before(:each) do
      @parser = Coffeetags::Parser.new ''
    end
    it 'gets the scope path for function' do
      @parser.scope_path(@cf_tree[1], @cf_tree[0...1] ).should == 'Campfire'
    end

    it 'gets the scope path for second function' do
      @parser.scope_path(@cf_tree[3], @cf_tree[0..2] ).should == 'Campfire'
    end

    it "gets the scope for nested function" do
      @parser.scope_path(@cf_tree[5], @cf_tree[0..4]).should == 'Campfire.handlers.resp'
    end

    it "gets the scope of a function which comes after nested function" do
      @parser.scope_path(@cf_tree[7], @cf_tree[0..6]).should == 'Campfire'
    end

    it 'gets scope for last method defined in diff class' do
      @parser.scope_path(@cf_tree.last, @cf_tree).should == 'Test'
    end
  end

  context 'Parsing' do
    context 'Scoping' do
      before(:each) do
        @coffee_parser = Coffeetags::Parser.new @campfire_class, true
        @test_parser = Coffeetags::Parser.new @test_file, true
        @coffee_parser.execute!
      end

      def cf_defined_values_should_equal(cf, c)
        cf.each do |k, v|
          c[k].should == v
        end
      end

      it "parses the class" do
        c = @coffee_parser.tree.find { |i| i[:name] == 'Campfire'}
        cf = @cf_tree.find {|i| i[:name] == 'Campfire'}
        cf_defined_values_should_equal cf, c
      end

      it "parses the 2nd class" do
        c = @coffee_parser.tree.find { |i| i[:name] == 'Test'}
        c.should == @cf_tree.find {|i| i[:name] == 'Test'}
      end

      it "parses the class with dot in name" do
        @coffee_parser = Coffeetags::Parser.new @class_with_dot, true
        @coffee_parser.execute!

        c = @coffee_parser.tree.find { |i| i[:name] == 'App.Campfire'}
        c.should == @cf_tree.find {|i| i[:name] == 'App.Campfire'}
      end

      it "parses the class with @ in name" do
        @coffee_parser = Coffeetags::Parser.new @class_with_at, true
        @coffee_parser.execute!

        c = @coffee_parser.tree.find { |i| i[:name] == 'Campfire'}
        cf = @cf_tree.find {|i| i[:name] == 'Campfire'}
        cf_defined_values_should_equal cf, c
      end

      it "parses the instance variable" do
        c = @coffee_parser.tree.find { |i| i[:name] == '@url'}
        c.should == @cf_tree.find {|i| i[:name] == '@url'}
      end

      it "parses the object literal with functions" do
        c = @coffee_parser.tree.find { |i| i[:name] == 'resp'}
        c.should == @cf_tree.find {|i| i[:name] == 'resp'}
      end

      it "parses a nested function" do
        c = @coffee_parser.tree.find { |i| i[:name] == 'onSuccess'}
        c.should == @cf_tree.find {|i| i[:name] == 'onSuccess'}
      end

      it "parses a method var" do
        c = @coffee_parser.tree.find { |i| i[:name] == 'url'}
        c.should == @cf_tree.find {|i| i[:name] == 'url'}
      end
    end
  end

  context 'Test.coffee parsing' do
    before(:each) do
      @parser_test = Coffeetags::Parser.new @test_file, true
      @parser_test.execute!
    end

    it "doesnt extract a variable from a tricky line" do
      @parser_test.tree.find { |i| i[:name] == 'Filter'}.should == nil
    end

    it 'correctly recognizes an object in if block' do
      pro = @parser_test.tree.find { |i| i[:name] == 'fu'}
      pro[:parent].should == '_loop'

      pro = @parser_test.tree.find { |i| i[:name] == 'nice'}
      pro[:parent].should == '_loop'
    end

    it 'correctly recognizes an object in for block' do
      pro = @parser_test.tree.find { |i| i[:name] == 'ugh'}
      pro[:parent].should == '_loop.element'

    end
      it "detects a fat arrow function" do
        c = @parser_test.tree.find { |i| i[:name] == 'bound_func'}
        c.should == @test_tree.find {|i| i[:name] == 'bound_func'}

      end
    it "extracts a method defined in a prototype" do
      pro = @parser_test.tree.find { |i| i[:name] == 'loop'}
      exp = @test_tree.find { |i| i[:name] == 'loop'}
      pro.should_not be nil
      pro.should == exp
    end

    it 'Ignores #TODO: -> (and any other comment line)' do
      @parser_test.tree.find { |i| i[:name] == 'TODO' }.should be_nil

    end

    context 'for loop' do
      subject {
        @parser_test = Coffeetags::Parser.new @test_file, true
        @parser_test.execute!
        @parser_test.tree.find { |i| i[:name] == 'element'}
      }
      it "finds variables defined in for loop" do
        subject[:line].should == 28
      end

      it "extracts the name" do
        subject[:name].should == 'element'
      end

      it "detects the scope" do

        subject[:parent].should == '_loop'
      end
    end
    context 'Block comments' do
      let(:bc_file) { 'spec/fixtures/blockcomment.coffee'}
      let(:bc_ctags_file) { 'spec/fixtures/blockcomment.ctags'}
      let(:blockcomment_file) { File.read(bc_file)}
      let(:blockcomment_tree) {
        [{:level=>0,
          :parent=>"window",
          :source=>"echo2 :-> console.log 'echo'",
          :line=>7,
          :kind=>"f",
          :name=>"echo2"},
          {:level=>0,
          :parent=>"window",
          :source=>"foo2 : (x) -> console.log 'bar \#{x}'",
          :line=>10,
          :kind=>"f",
          :name=>"foo2"},
          {:level=>0,
          :parent=>"window",
          :source=>"baz : (x, y) ->",
          :line=>15,
          :kind=>"f",
          :name=>"baz"},
          {:level=>2,
           :parent=>"baz",
           :source=>"  console.log 'baz \#{x} : \#{y}'",
           :line=>16,
           :kind=>"o",
           :name=>""},
          {:level=>0,
          :parent=>"window",
          :source=>"echo3 :-> console.log 'echo'",
          :line=>23,
          :kind=>"f",
          :name=>"echo3"},
          {:level=>0,
          :parent=>"window",
          :source=>"foo : (x) -> console.log 'bar \#{x}'",
          :line=>26,
          :kind=>"f",
          :name=>"foo"}]
      }

      subject {
        parser = Coffeetags::Parser.new(blockcomment_file, true)
        parser.execute!
        parser
      }

      it 'ignores block comments when parsing the contents' do
        subject.tree.zip(blockcomment_tree).each do |subject_node, blockcomment_node|
          subject_node.should == blockcomment_node
        end
      end


      context 'top-down test' do
        let(:blockcomment_ctags){ File.read(bc_ctags_file) }
        subject {
          Coffeetags::Utils.run({ :output => 'test_bc.out', :files => bc_file})
          File.read 'test_bc.out'
        }

        after(:all) { `rm test_bc.out` }

        it 'ignores block comments in output' do
          subject.should == blockcomment_ctags
        end
      end
    end
  end
end

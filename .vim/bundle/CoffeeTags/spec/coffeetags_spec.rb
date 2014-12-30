require 'spec_helper'
include Coffeetags
describe Utils do
  context 'Argument parsing' do
    it "returns nil when nothing is passed" do
      Utils.option_parser([]).should == { :exit => true, :files => [] }
    end

    it "returns files list" do
      Utils.option_parser(['lol.coffee']).should == {:files => ['lol.coffee']}
    end

    it "parses --include-vars option" do
      Utils.option_parser( [ '--include-vars',  'lol.coffee']).should == { :include_vars => true, :files => ["lol.coffee"]}
    end

    it "parses --append option" do
      Utils.option_parser( ['--append',  'lol.coffee']).should == { :append => true, :files => ["lol.coffee"]}
    end

    it "parses --tag-relative option" do
      Utils.option_parser( [ '--tag-relative', 'lol.coffee']).should == { :tag_relative => true, :files => ['lol.coffee'] }
    end

    it "parses -f <file> option" do
      Utils.option_parser( [ '-f','tags' ,'lol.coffee']).should == { :output => 'tags', :files => ['lol.coffee'] }
    end

    it "outputs list-kinds to console" do
      options = nil
      output = capture_stdout {
        options = Utils.option_parser( ['--list-kinds'])
      }
      output.should == <<-TAG
f  function
c  class
o  object
v  var
p  proto
b  block
      TAG

      options[:exit].should == true
    end
  end

  context 'Relative file path' do
    it "returns file with nil output" do
      Utils.file_path("some/file", nil, nil).should == "some/file"
    end

    it "returns file with non relative path" do
      Utils.file_path("some/file", "some/output", nil).should == "some/file"
    end

    it "returns relative path to file from output" do
      Utils.file_path("/some/path/to/file", "/some/path/for/output", true).should == "../to/file"
    end

    it "returns relative path from cwd" do
      Utils.file_path("some/path/to/file", ".git/tags", true).should == "../some/path/to/file"
    end
  end

  context 'Parser runner' do

    before do
      @fake_parser = mock('Parser')
      @fake_parser.stub! :"execute!"
      @fake_parser.stub!( :tree).and_return []

      Parser.stub!(:new).and_return @fake_parser

      @fake_formatter = mock 'Formatter'
      @fake_formatter.stub! :parse_tree
      @fake_formatter.stub!(:lines).and_return %w{ tag tag2 tag3 }
      @fake_formatter.stub!(:tags).and_return <<-TAG
tag
tag2
tag3
      TAG
      Coffeetags::Formatter.stub!(:new).and_return @fake_formatter
      Coffeetags::Formatter.stub!(:header).and_return "header\n"
      File.stub!(:read).and_return 'woot@'
    end


    it "opens the file and writes tags to it" do
      Utils.run({ :output => 'test.tags', :files => ['woot'] })

      `cat test.tags`.should== <<-FF
header
tag
tag2
tag3
FF

    end

    after :each do
      `rm test.tags`
    end

  end

  context 'Runner exit' do
    it 'exits when the exit flag is present' do
      expect {
        Coffeetags::Utils.run({ :exit => true })
      }.to raise_error SystemExit
    end
  end

  context "Complete output" do
    it "generates tags for given file" do
      Coffeetags::Utils.run({ :output => 'test.out', :files => 'spec/fixtures/test.coffee' })

      File.read("test.out").should == File.read("./spec/fixtures/out.test.ctags")
    end

    it "generates tags for given files" do
      Coffeetags::Utils.run({ :output => 'test.out', :files => ['spec/fixtures/test.coffee', 'spec/fixtures/campfire.coffee'] })

      File.read("test.out").should == File.read("./spec/fixtures/out.test-two.ctags")

    end

    it "generates tags with relative path from tags file" do
      files = [ "spec/fixtures/test.coffee", 'spec/fixtures/campfire.coffee']

      FileUtils.mkdir "testout" unless File.directory? "testout"
      output = "testout/test.out"

      Coffeetags::Utils.run({ :output => output, :files => files, :tag_relative => true })

      File.read(output).should == File.read("./spec/fixtures/out.test-relative.ctags")
    end

    it "appends tags for given file" do
      FileUtils.cp 'spec/fixtures/append.ctags', 'test.out'

      Coffeetags::Utils.run({ :output => 'test.out', :files => ['spec/fixtures/campfire.coffee'], :append => true })

      File.read("test.out").should == File.read("./spec/fixtures/append-expected.ctags")
    end

    it "appends tags with tag relative for given file" do
      FileUtils.mkdir "testout" unless File.directory? "testout"
      output = "testout/test.out"

      FileUtils.cp "spec/fixtures/out.test-relative-append.ctags", output

      lines = Coffeetags::Utils.run({ :output => output, :files => ["spec/fixtures/campfire.coffee"], :append => true, :tag_relative => true })
      File.read(output).should == File.read("./spec/fixtures/out.test-relative.ctags")
    end

    after :each do
      `rm test.out` if File.exists? 'test.out'
      `rm -rf testout` if File.directory? 'testout'
    end
  end

  context "setup tag lines" do
    it "returns empty array when append flag is false" do
      Coffeetags::Utils.setup_tag_lines("spec/fixtures/out.test-two.ctags", ["spec/fixtures/test.coffee"], false).should == []
    end

    it "returns empty array for nil output" do
      Coffeetags::Utils.setup_tag_lines(nil, nil, true).should == []
    end

    it "returns empty array for nil output" do
      Coffeetags::Utils.setup_tag_lines("file/does/not/exist", nil, true).should == []
    end

    it "returns contents of output file without header" do
      lines = Coffeetags::Utils.setup_tag_lines("spec/fixtures/blockcomment.ctags", nil, true).map {|l| l.split("\t")[0]}
      lines.should == %w{baz echo2 echo3 foo foo2}
    end

    it "returns contents of output file without header and without tags for files that will be indexed" do
      lines = Coffeetags::Utils.setup_tag_lines("spec/fixtures/out.test-two.ctags", ["spec/fixtures/test.coffee"], true).map {|l| l.split("\t")[0]}
      lines.should == %w{Campfire Test bump constructor handlers onFailure onSuccess recent resp roomInfo rooms}
    end

    it "returns contents of output file with relative file paths without header and without tags for files that will be indexed" do
      FileUtils.mkdir "testout" unless File.directory? "testout"
      output = "testout/test.out"

      FileUtils.cp "spec/fixtures/out.test-relative.ctags", output

      lines = Coffeetags::Utils.setup_tag_lines(output, ["spec/fixtures/test.coffee"], true).map {|l| l.split("\t")[0]}
      lines.should == %w{Campfire Test bump constructor handlers onFailure onSuccess recent resp roomInfo rooms}
    end

    it "returns contents of output file with relative file paths from absolute file path" do
      FileUtils.mkdir "testout" unless File.directory? "testout"
      output = "testout/test.out"

      FileUtils.cp "spec/fixtures/out.test-relative.ctags", output

      expanded_path = Pathname.new("spec/fixtures/test.coffee").expand_path.to_s

      lines = Coffeetags::Utils.setup_tag_lines(output, [expanded_path], true).map {|l| l.split("\t")[0]}
      lines.should == %w{Campfire Test bump constructor handlers onFailure onSuccess recent resp roomInfo rooms}
    end

    it "returns contents of output file with relative file paths from absolution output path" do
      FileUtils.mkdir "testout" unless File.directory? "testout"
      output = "testout/test.out"

      FileUtils.cp "spec/fixtures/out.test-relative.ctags", output

      lines = Coffeetags::Utils.setup_tag_lines(Pathname.new(output).expand_path.to_s, ["spec/fixtures/test.coffee"], true).map {|l| l.split("\t")[0]}
      lines.should == %w{Campfire Test bump constructor handlers onFailure onSuccess recent resp roomInfo rooms}
    end
  end

end

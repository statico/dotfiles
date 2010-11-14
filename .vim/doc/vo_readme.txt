*vo_readme.txt*   VimOutliner 0.3.4 for Vim 6.1+       *vo* *vimoutliner*

Contents

    LICENSE                                                      |vo-license|
    VERSION                                                      |vo-version|
    INSTALLING AND TESTING VIMOUTLINER                           |vo-install|
        Automatic method                                    |vo-auto-install|
        Updating an existing installation                       |vo-updating|
        Manual method                                     |vo-manual-install|
        Color schemes                                              |vo-color|
        Testing                                                  |vo-testing|
        Debian                                                    |vo-debian|
    USING VIMOUTLINER ON OTHER FILE TYPES                    |vo-other-files|
    TROUBLESHOOTING                                      |vo-troubleshooting|
    VIMOUTLINER PHILOSOPHY                                    |vo-philosophy|
    RUNNING VIMOUTLINER                                          |vo-running|
        What are the comma comma commands                        |vo-command|
        How do you perform basic VimOutliner activities       |vo-activities|
	Menu                                                        |vo-menu|
        Vim Outliner objects                                     |vo-objects|
	Post Processors                                  |vo-post-processors|
    CAUTIONS!!!                                                 |vo-cautions|
    ADVANCED                                                    |vo-advanced|                                                
        Executable Lines                                |vo-executable-lines|
    PLUGINS                                                      |vo-plugins|
        Checkboxes                                              |vo-checkbox|
        Hoisting                                                |vo-hoisting|
    SCRIPTS                                                      |vo-scripts|
        vo_maketags.pl                                          |vo-maketags|
        otl2html.py                                                |otl2html|
    OTHER INFORMATION                                         |vo-other-info|

-------------------------------------------------------------------------
LICENSE                                                      *vo-license*

    VimOutliner Copyright (C) 2001, 2003 by Steve Litt
                Copyright (C) 2004 by Noel Henson
    Licensed under the GNU General Public License (GPL), version 2
    Absolutely no warranty, see COPYING file for details.
    
    If your distro doesn't come with this file
        HTML: http://www.gnu.org/copyleft/gpl.html
        Text: http://www.gnu.org/copyleft/gpl.txt 

-------------------------------------------------------------------------
VERSION                                                      *vo-version*

    Version 0.3.4
    Released 
   
    Additions/changes:
        Added ,,cp which works like ,,c% but forces % signs to be added
        Color schemes have been added
        Checkboxes and hoisting default to 'on'
	Modified ,,cb and ,,c% (and ,,cp) to work only on headings
	Fixed the ,,cz command to make the correct call
	Added descriptions of VO objects to help 
	  (headings, text, tables, etc.)

    Bug fixes:
        W18 errors have been fixed

-------------------------------------------------------------------------
INSTALLING AND TESTING VIMOUTLINER                           *vo-install*

    How do I install VimOutliner?
        Automatic Method                |vo-auto-install|
        Updating                        |vo-updating|
        Manual Method                   |vo-manual-install|
        Testing                         |vo-testing|

        Automatic method                            *vo-auto-install*

             The new automatic installation targets Unix-compatible
             platforms. 
             $ tar xzvf vimoutliner-0.3.x.tar.gz
             $ cd vimoutliner
             $ sh install.sh

             First you can decide whether to install the VimOutliner
             files or abort the process leaving everything unchanged.
             Assuming you confirmed the installation, the script
             creates the necessary directory tree and copies the files
             which provide the core functionality and documentation.

             With the second question you decide whether you want to
             install some brand new add-ons, currently implementing
             hoisting and checkboxes.

        Updating an existing installation                   *vo-updating*

             Updating an existing installation might require some
             manual work.

             If you are already working with a previous VimOutliner
             release, there is a slight chance that the current directory
             tree is different from your current one. In this case, you
             will have to manually migrate your files to the new locations.

             The installation script creates unique backups of files
             being replaced with newer versions. So if you put some
             local customisations into, say $HOME/.vimoutlinerrc, you'll
             probably have to merge the backup with the new file by hand.
            

        Manual method                                 *vo-manual-install*

            You can also copy the files from the unpacked distribution
            tarball into their destination folders by yourself. The
            following steps are a description of what has to go where
            and assume some knowledge of your vim setup.
            
            If you encounter problems, please contact the mailinglist
            for an immediate solution and more complete future
            documentation. www.lists.vimoutliner.org
            
            If you want to setup VimOutliner on a system running Microsoft
            Windows, the directory $HOME denotes the base folder of the
            vim installation.  If you're on Unix based system, $HOME
            is as usual.

            You need the following subtrees in your $HOME directory:
              $HOME/.vim/
                  doc/  ftdetect/  ftplugin/  syntax/
              $HOME/.vimoutliner/
                  plugins/  scripts/

            The distribution tarball unpacks into a directory vimoutliner
            with the following contents
              add-ons/
                plugins/               (2)
                scripts/               (2)
              doc/                     (1)
              ftdetect/                (1)
              ftplugin/                (1)
              install.sh*
              scripts/                 (2)
              syntax/                  (1)
              vimoutlinerrc            (3)

            (1) The content of these folders should be copied to their
                namesakes in the $HOME/.vim folder
            (2) The content of these folders should be copied to their
                namesakes in the $HOME/.vimoutliner folder
            (3) This file needs to be moved to $HOME/.vimoutlinerrc

            Your $HOME/.vimrc file should contain the lines
                 filetype plugin indent on
                 syntax on

            Your $HOME/.vim/ftplugin/vo_base.vim file should contain
            the lines
                 runtime! ftdetect/*.vim
                
            Finally, you need to integrate the online help provided
            with VimOutliner into the vim help system.  Start vim
            and execute the following command:
                :helptags $HOME/.vim/doc

            At this point, VimOutliner should be functional.
            Type "help vo" to get started.
            
    Color Schemes                                              *vo-color*
   
            Color schemes specify the colors Vim Outliner uses when 
        displaying an outline. Colors are specified by object and level.
        These objects currently include: headings, body text, pre-
        formatted body text, tables and others. See |vo-objects| for
        more information.

        Color scheme files are located in the system-wide vim colors
        directory and/or your $HOME/.vim/colors directory. You can
        select from any of the provided schemes. Vim Outliner internally
        includes a scheme the matches vo_light.vim.

        To override the default color scheme you can edit these lines
        in your $HOME/.vimoutlinerrc file:
        
            "Custom Colors **********************************
            " Uncomment this next line to force using VO on 
            " a light background
            " colorscheme vo_light 
            " Uncomment this next line to force using VO on 
            " a dark background
            " colorscheme vo_dark 

        To create your own scheme follow these simple steps:

            1. Move to your $HOME/.vim/colors directory.
               If you don't have one, create it.

            2. Make a copy of one of the included schemes to use
               as a starting point. You should be able to find them
               in places like: $HOME/.vim/colors and 
               /usr/share/vim/vim63/colors. Put the copy in your
               own colors directory ($HOME/.vim/colors)

            3. Edit the scheme file to change whatever colors you
               wish.

            4. Select the scheme in your $HOME/.vimoutlinerrc file.
               The line should look something like this:

               colorscheme noel

        That's all there is to it.

    Testing base functionality                               *vo-testing*

        rm $HOME/vo_test.otl
        gvim $HOME/vo_test.otl
            or vim $HOME/vo_test.otl
        Verify the following:
            Tabs indent the text
            Different indent levels are different colors
            Lines starting with a colon and space word-wrap
                 Lines starting with colons are body text. They should
                 word wrap and should be a special color (typically
                 green, but it can vary). Verify that paragraphs of body
                 text can be reformatted with the Vim gq commands.

    Verify interoutline linking

        Interoutline linking currently requires a working perl installation
        to generate the necessary tag file. We are looking into porting
        this to vim's own scripting language.

        Place the following two lines in $HOME/vo_test.otl:
            _tag_newfile
                $HOME/vo_newfile.otl
        Note that in the preceding, the 2nd line should be indented
        from the first.

        To create VimOutliner's tag file $HOME/.vimoutliner/vo_tags.tag,
        run vo_maketags.pl, which resides in $HOME/.vimoutliner/scripts/:
            $ $HOME/.vimoutliner/scripts/vo_maketags.pl $HOME/vo_test.otl

        In $HOME/vo_test.otl
            Cursor to the _tag_newfile marker
            Press Ctrl+K
                You should be brought to $HOME/vo_newfile.otl 
            Press Ctrl+N
                You should be brought back to $HOME/vo_test.otl 
            Note:
                Ctrl+K is a VimOutliner synonym for Ctrl+]
                Ctrl+N is a VimOutliner synonym for Ctrl+T

    Debian Installation                                       *vo-debian*

    	Debian does include Vim Outliner as a package. However some
	Debian version require this line to be added to your .vimrc file:

	syntax on

-------------------------------------------------------------------------
USING VIMOUTLINER ON OTHER FILE TYPES                    *vo-other-files*

    How do I use VimOutliner on non .otl files

        Overview
             Previous VimOutliner versions used the ol script to invoke
             VimOutliner. As of VimOutliner 0.3.0, the ol script is no
             longer necessary nor provided. Instead, VimOutliner is now a
             Vim plugin, so Vim does all the work.
            
             This makes VimOutliner much simpler to use in most cases,
             but Vim plugins are file extension based, meaning that if
             you want to use VimOutliner on a file extension other than
             .otl, you must declare that file extension in
             $HOME/.vim/ftdetect/vo_base.vim. In this section we'll
             use the .emdl extension (Easy Menu Definition Language)
             as an example.

        To enable VimOutliner work with .emdl files, do this:
            vim $HOME/.vim/ftdetect/vo_base.vim
            Right below the line reading:
                au! BufRead,BufNewFile *.otl        setfiletype vo_base
            Insert the following line:
                au! BufRead,BufNewFile *.emdl        setfiletype vo_base
            Save and exit
            Test with the following:
                gvim $HOME/vo_test.emdl
            You should get
                level colors,
                body text (lines starting with colon)
                comma comma commands (try ,,2 and ,,1)

-------------------------------------------------------------------------
TROUBLESHOOTING                                      *vo-troubleshooting*

    Troubleshooting

        I can't switch between colon based and space based body text
            See next question

        My ,,b and ,,B don't do anything. How do I fix it?
            vim $HOME/.vim/ftplugin/vo_base.vim
            Search for use_space_colon
            Make sure it is set to 0, not 1
            Rerun Vim, and ,,b and ,,B should work

        I don't get VimOutliner features on files of extension .whatever
            vim $HOME/.vim/ftdetect/vo_base.vim
            Right below the line reading:
                au! BufRead,BufNewFile *.otl          setfiletype vo_base
            Insert the following line:
                au! BufRead,BufNewFile *.whatever     setfiletype vo_base
            Save and exit

-------------------------------------------------------------------------
VIMOUTLINER PHILOSOPHY                                    *vo-philosophy*

    Authoring Speed
        VimOutliner is an outline processor with many of the same
        features as Grandview, More, Thinktank, Ecco, etc. Features
        include tree expand/collapse, tree promotion/demotion, level
        sensitive colors, interoutline linking, and body text.
        
        What sets VimOutliner apart from the rest is that it's been
        constructed from the ground up for fast and easy authoring.
        Keystrokes are quick and easy, especially for someone knowing the
        Vim editor. The mouse is completely unnecessary (but is supported
        to the extent that Vim supports the mouse). Many of the
        VimOutliner commands start with a double comma because that's
        very quick to type.
        
        Many outliners are prettier than VimOutliner. Most other
        outliners are more intuitive for the newbie not knowing Vim. Many
        outliners are more featureful than VimOutliner (although
        VimOutliner gains features monthly and is already very powerful).
        Some outliners are faster on lookup than VimOutliner. But as far
        as we know, NO outliner is faster at getting information out of
        your mind and into an outline than VimOutliner.
        
        VimOutliner will always give you lightning fast authoring. That's
        our basic, underlying philosophy, and will never change, no
        matter what features are added.

    Vim integration
        Earlier VimOutliner versions prided themselves on being
        standalone applications, self-contained in a single directory
        with a special script to run everything.
        
        As of 0.3.0, VimOutliner is packaged as a Vim Plugin, eliminating
        the need for the ol script, which many saw as clumsy. Given that
        all VimOutliner features are produced by the Vim engine, it makes
        perfect sense to admit that VimOutliner is an add-on to Vim.
        
        Therefore VimOutliner now prides itself in being a Vim plugin.
        With the VimOutliner package installed, the Vim editor yields the
        VimOutliner feature set for files whose extensions are listed as
        vo_base types in $HOME/.vim/ftplugin/vo_base.vim.
        
        The Vim Plugin philosophy yields several benefits:
            Less reliance on Perl, bash and environment vars
            (upcoming) Portability between Linux, Windows and Mac
            (upcoming) Installation via Vim script

-------------------------------------------------------------------------
RUNNING VIMOUTLINER                                          *vo-running*

    Vim knowledge is a prerequisite
        Overview
            You needn't be a Vim expert to use VimOutliner. If you know
            the basics -- inserting and deleting linewise and
            characterwise, moving between command and insert modes, use
            of Visual Mode selections,and reformatting, you should be
            well equipped to use VimOutliner.
            
            VimOutliner is a set of Vim scripts and configurations. Its
            features all come from the Vim editor's engine. If you do not
            know Vim, you'll need to learn the Vim basics before using
            VimOutliner. Start by taking the Vim tutorial. The tutorial
            should take about 2 hours.
            
            VimOutliner is so fast, that if you often use outlining,
            you'll make up that time within a week.

        Taking the Vim tutorial
            Run vim or gvim
            Type the command, :help tutor
            Follow the instructions

    What are the comma comma commands                        *vo-command*
        Overview
            For maximum authoring speed, VimOutliner features are
            accessed through keyboard commands starting with 2 commas.
            The double comma followed by a character is incredibly fast
            to type.
            
            We expect to create more comma comma commands, so try not to
            create your own, as they may clash with later comma comma
            commands. If you have an exceptionally handy command, please
            report it to the VimOutliner list. Perhaps others could
            benefit from it.

        Command list
            ,,D   all      VimOutliner reserved command
            ,,H   all      reserved for manual de-hoisting (add-on)
            ,,h   all      reserved for hoisting (add-on)
            ,,1   all      set foldlevel=0
            ,,2   all      set foldlevel=1
            ,,3   all      set foldlevel=2
            ,,4   all      set foldlevel=3
            ,,5   all      set foldlevel=4
            ,,6   all      set foldlevel=5
            ,,7   all      set foldlevel=6
            ,,8   all      set foldlevel=7
            ,,9   all      set foldlevel=8
            ,,0   all      set foldlevel=99999
            ,,-   all      Draw dashed line
            ,,f   normal   Directory listing of the current directory
            ,,s   normal   Sort sub-tree under cursor ascending
            ,,S   normal   Sort sub-tree under cursor descending
            ,,t   normal   Append timestamp (HH:MM:SS) to heading
            ,,T  normal   Pre-pend timestamp (HH:MM:SS) to heading
            ,,T   normal   Pre-pend timestamp (HH:MM:SS) to heading
            ,,t   insert   Insert timestamp (HH:MM:SS) at cursor
            ,,d   normal   Append datestamp  (YYYY-MM-DD) to heading
            ,,d   insert   Insert datestamp  (YYYY-MM-DD) at cursor
            ,,D   normal   Pre-pend datestamp  (YYYY-MM-DD) to heading
            ,,B   normal   Make body text start with a space
            ,,b   normal   Make body text start with a colon and space
            ,,w   insert   Save changes and return to insert mode
            ,,e   normal   Execute the executable tag line under cursor

    What are some other VimOutliner Commands

        Overview
            Naturally, almost all Vim commands work in VimOutliner.
            Additionally, VimOutliner adds a few extra commands besides
            the comma comma commands discussed previously.

        Command list:
            Ctrl+K        Follow tag (Synonym for Ctrl+])
            Ctrl+N        Return from tag (Synonym for Ctrl+T)
            Q             Reformat (Synonym for gq)

    How do you perform basic VimOutliner activities       *vo-activities*

        How do I collapse a tree within command mode?
            zc
            (note: a full list of folding commands |fold-commands|)

        How do I expand a tree within command mode?
            To expand one level:
                zo
            To expand all the way down
                zO

        How do I demote a headline?
            In command mode, >>
            In insert mode at start of the line, press the Tab key
            In insert mode within the headline, Ctrl+T

        How do I promote a headline?
            In command mode, <<
            In insert mode at start of the line, press the Backspace key
            In insert mode within the headline, Ctrl+D

        How do I promote or demote several consecutive headlines?
            Highlight the lines with the V command
            Press < to promote or > to demote. You can precede
            the < or > with a count to promote or demote several levels

        How do I promote or demote an entire tree?
            Collapse the tree
            Use << or >> as appropriate

        How do I collapse an entire outline?
            ,,1

        How do I maximally expand an entire outline?
            ,,0

        How do I expand an outline down to the third level?
            ,,3

        How do I move a tree?
            Use Vim's visual cut and paste

        How do I create body text?
            Open a blank line below a headline
            Start the line with a colon followed by a space
            Continue to type. Your text will wrap

        How do I reformat body text?
            Highlight (Shift+V) the body text to be reformatted
            Use the gq command to reformat

        How do I reformat one paragraph of body text?
            The safest way is highlighting.
                DANGER! Other methods can reformat genuine headlines.

        How do I switch between colon based and space based body text?
            ,,b for colon based, ,,B for space based

        What if ,,b and ,,B don't work
            Change variable use_space_colon from 1 to 0
                in $HOME/.vim/ftplugin/vo_base.vim

        How do I perform a wordcount?
            Use the command :w !wc
                The space before the exclamation point is a MUST.

    Menu                                                        *vo-menu*
    
	There is a simple menu included in Vim Outliner when running
	in GUI mode. Named 'VO', you can usually find it right next to the
	'Help' menu. There are commands to change the fold level and select 
	alternate color schemes. There is also entries for common tools.
	Currently there are only two entries for running the otl2html.py
	script included with VO. 
	
	The first otl2html.py tool item executes the script with default 
	options. The only control a user has it to modify the nnnnnn.css 
	file.

	The second tool item calls a shell script, 'myotl2html.sh'. This
	script should be provided by the user and is not included in VO
	releases. A sample myotl2html.sh script might look like this:

	#!/bin/bash
	otl2html.py -S pjtstat.css $1 > $HOME/public_html/$1.html 

	If you have several different types of reports you create regularly,
	you can create your own menu entries. Just add lines like these to
	your ~/.vimoutlinerrc file:

	amenu &VO.&Reports.&Big\ Project :!otl2html.py -S big.css % > %.html
	amenu &VO.&Reports.&Hot\ List :!otl2html.py -S todo.css % > %.html
	amenu &VO.&Reports.&Weekly :!otl2html.py -S weekly.css % > %.html

	I'm sure you get the idea.

    Vim Outliner Objects                                     *vo-objects*
            
        There are several object/line types that VO supports. The most
        common on simple headings and body text. Simple headings are
        tab-indented line that start with any non-whitespace character 
        except: : ; | < >. These characters specify other objects. Here
        is a list of each of the non-heading types:
            :        body text (wrapping)
            ;        preformatted body text (non-wrapping)
            |        table
            >        user-defined, text block (wrapping)
            <        user-defined, preformatted text block (non-wrapping)
        
        The body text marker, :, is used to specify lines that are
        automatically wrapped and reformatted. VO and post-processors are
        free to wrap and reformat this text as well as use proportionally-
        spaced fonts. A post-processor will probably change the appearance
	of what you have written. If you are writing a book or other 
	document, most of the information you enter will be body text.

        Here is an example:

	    Kirby the Wonder Dog
	    	: Kirby is nine years old. He understand about 70-100
		: English words. Kirby also understands 11 different hand
		: signals. He is affectionate, playful and attentive.
		:
		: His breeding is unknown. He appears to be a mix between
		: a german shepherd and a collie.
	
	When folded, body text looks something like this:

	    Kirby the Wonder Dog
		[TEXT] -------------------------------- (6 lines)
	
	The preformatted text marker, ;, is used to mark text that should
	not be reformatted nor wrapped by VO or any post-processor. A post-
	processor would use a fixed-space font, like courier, to render 
	these lines. A post-processor will probably not change the 
	appearance of what you have written. This is useful for making text 
	picture, program code or other format-dependent text.

	Here is an example:

	    Output waveform
		;         _______                ______
		;   _____/       \______________/     
		;        |-10us--|----35us------|
	
	When folded, preformatted body text looks something like this:

	    Output waveform
		[TEXT BLOCK] -------------------------- (6 lines)
	
	The table marker, |, is used to create tables. This is an excellent
	way to show tabular data. The marker is used as if it were are real
	vertical line. A || (double-|) is optionally used to mark a table
	heading line. This is useful for post-processors.

	Here is an example:

		Pets
			|| Name  | Age | Animal | Inside/Outside |
			| Kirby  |   9 |    dog |           both |
			| Hoover |   1 |    dog |           both |
			| Sophia |   9 |    cat |         inside |
	
	There is no automatic alignment of columns yet. It must be done
	manually. The post-processor, otl2thml.py, does have alignment
	functions. See its documentation for more information.
	
	When folded, a table looks something like this:

	    Pets
		[TABLE] ------------------------------- (4 lines)
	
	User-defined text is similar to body text but more flexible and it's
	use is not pre-defined by Vim Outliner. The basic, user-defined 
	text block marker, >, behaves just like body text. 
		
        For example:

	    Kirby the Wonder Dog
	    	> Kirby is nine years old. He understand about 70-100
		> English words. Kirby also understands 11 different hand
		> signals. He is affectionate, playful and attentive.
		>
		> His breeding is unknown. He appears to be a mix between
		> a german shepherd and a collie.
	
	When folded, body text looks something like this:

	    Kirby the Wonder Dog
		[USER] -------------------------------- (6 lines)
	
	But unlike body text, user-defined text can be expanded. You could
	have user-defined text types. If you were writing a book, in 
	addition to body text for paragraphs you might need special 
	paragraphs for tips and warnings. User-defined text blocks can 
	accomplish this:

		>Tips
		> Don't forget to back up your computer daily. You don't 
		> need to back up the entire computer. You just need to 
		> backup up the files that have changed.
		>Warning
		>Never store you backup floppy disks on the side of you 
		>file cabinets by adhering them with magnets.
		
	A post processor will know how to remove the style tags (Tips and 
	Warning) and you want the text to be formatted.

	When folded, the above would appear as:

		[USER Tips] --------------------------- (4 lines)
		[USER Warning]------------------------- (3 lines)
	
	The user-defined, preformatted text block marker, <, behaves just
	like preformatted text. But like >, it leaves the functional
	definition up to the user. A simple user-defined, preformatted text 
	block could be:

	    Tux
		<                 _.._                   
		<              .-'    `-.                
		<             :          ;               
		<             ; ,_    _, ;               
		<             : \{"  "}/ :               
		<            ,'.'"=..=''.'.              
		<           ; / \      / \ ;             
		<         .' ;   '.__.'   ; '.           
		<      .-' .'              '. '-.        
		<    .'   ;                  ;   '.      
		<   /    /                    \    \     
		<  ;    ;                      ;    ;    
		<  ;   `-._                  _.-'   ;    
		<   ;      ""--.        .--""      ;     
		<    '.    _    ;      ;    _    .'      
		<    {""..' '._.-.    .-._.' '..""}      
		<     \           ;  ;           /       
		<      :         :    :         :        
		<      :         :.__.:         :        
		<       \       /"-..-"\       /    fsc  
		<        '-.__.'        '.__.-'          

	When folded it would be:

	    Tux
		[USER BLOCK] -------------------------- (6 lines)

	Like user-defined text, these blocks can be given user-defined 
	styles. For example:

		<ASCIIart
		<                 _.._                   
		<              .-'    `-.                
		<             :          ;               
		<             ; ,_    _, ;               
		<             : \{"  "}/ :               
		<            ,'.'"=..=''.'.              
		<           ; / \      / \ ;             
		<         .' ;   '.__.'   ; '.           
		<      .-' .'              '. '-.        
		<    .'   ;                  ;   '.      
		<   /    /                    \    \     
		<  ;    ;                      ;    ;    
		<  ;   `-._                  _.-'   ;    
		<   ;      ""--.        .--""      ;     
		<    '.    _    ;      ;    _    .'      
		<    {""..' '._.-.    .-._.' '..""}      
		<     \           ;  ;           /       
		<      :         :    :         :        
		<      :         :.__.:         :        
		<       \       /"-..-"\       /    fsc  
		<        '-.__.'        '.__.-'          
		<Code
		< getRXDN macro
		< 
		< 	local	gRXD1, gRXD2
		< 	bcf	STATUS,C
		< 	btfsc	FLAGS,SERPOL
		< 
		< 	goto	gRXD1
		< 	btfsc	RXDN
		< 	bsf	STATUS,C
		< 	goto	gRXD2
		< 
		< gRXD1	btfss	RXDN
		< 	bsf	STATUS,C
		< 	nop
		< gRXD2
		< 	endm
	
	When folded, the above would appear as:

		[USER BLOCK ASCIIart] ----------------- (22 lines)
		[USER BLOCK Code] --------------------- (17 lines)

    Vim Outliner Post-processors                     *vo-post-processors*

	There are already serveral post processors for Vim Outliner. Some 
	are general purpose in nature and others perform specific 
	conversions. Here is list of currently know programs:

	Node.pm		Steve Litt    www.troubleshooters.com/projects/Node
            
			This program is general purpose in nature and can 
			generate many types of output. It can even output 
			complete keyboarder-friendly menu scripts (EDML
			www.troubleshooters.com/projects/edml). See the 
			website for more details.

	otl2docbook.pl	Christian Warden     www.vimoutliner.org  Downloads

			This perl script converts OTL files to DocBook.

	otl2html.py	Noel Henson	included with Vim Outliner
			This python script outputs html pages, one page per 
			OTL file. It is useful for creating pretty reports 
			and nicely formatted, but simple, web pages. See 
			the help (otl2thml.py --help) for more detail.

	otl2ooimpress   Noel Henson          www.vimoutliner.org  Downloads

			A very simple script to output Open Office Impress 
			files.

	otl2pdb.pl	Gabriel Horner       www.vimoutliner.org  Downloads

			A perl script to convert contact information in VO 
			to Palm pdb files.

	vo2html.py	Ricardo Cardenes     www.vimoutliner.org  Downloads

			Converts an OTL file to a set of html 'slides'. It 
			supports used-defined templates.

	otl_handler	Mahlon E. Smith  www.martini.nu/misc/otl_hander.tgz   

			Not a converter really, but a mod_perl script that 
			enables Apache to show VO todo lists in 
			a nicely-formatted way. Folding of sections and CSS 
			is supported.
			
	This list is likely to be out of date. Please check the Vim 
	Outliner website for more.

-------------------------------------------------------------------------
CAUTIONS!!!                                                 *vo-cautions*


-------------------------------------------------------------------------
ADVANCED VIMOUTLINER                                        *vo-advanced*

    Executable Lines                                *vo-executable-lines*

    Executable lines enable you to launch any command from a specially 
    constructed headline within VimOutliner. The line must be constructed 
    like this:
 
        Description _exe_ command

    Here's an example to pull up Troubleshooters.Com:
 
        Troubleshooters.Com _exe_ mozilla http://www.troubleshooters.com

    Executable lines offer the huge benefit of a single-source knowledge 
    tree, where all your knowledge, no matter what its format, exists 
    within a single tree of outlines connected with inter-outline links and 
    executable lines.
 
 To enable this behavior, insert the following code into your $HOME/.vimoutlinerrc file:
-------------------------------------------------------------------------
PLUGINS                                                      *vo-plugins*

    The VimOutliner distribution currently includes two plugins
    for easy handling of checkboxes and to enable hoisting (see below).

    If you want to check out other plugins or experimental stuff,
    take a look at VimOutliner's home page http://www.vimoutliner.org

    You can find more complete descriptions in your $HOME/.vimoutliner/doc
    folder, what follows here are the "just the facts".

        Checkboxes                                          *vo-checkbox*

            Checkboxes enable VimOutliner to understand tasks and calculate
            the current status of todo-lists etc. Three special notations
            are used:

            [_]     an unchecked item or incomplete task
            [X]     a checked item or complete task
            %       a placeholder for percentage of completion

            Several ,,-commands make up the user interface:

            ,,cb  Insert a check box on the current line or each line
                  of the currently selected range (including lines in
                  selected but closed folds). This command is currently
                  not aware of body text. Automatic recalculation of
                  is performed for the entire root-parent branch that
                  contains the updated child. (see ,,cz)
            ,,cx  Toggle check box state (percentage aware)
            ,,cd  Delete check boxes
            ,,c%  Create a check box with percentage placeholder except
	          on childless parents
            ,,cp  Create a check box with percentage placeholder on all
	          headings
            ,,cz  Compute completion for the tree below the current
                  heading.

           How do I use it?

               Start with a simple example.

               Let's start with planning a small party; say a barbeque.

               1. Make the initial outline

                   Barbeque
                       Guests
                           Bill and Barb
                           Larry and Louise
                           Marty and Mary
                           Chris and Christine
                           David and Darla
                           Noel and Susan
                       Food
                           Chicken
                           Ribs
                           Corn on the cob
                           Salad
                           Desert
                       Beverages
                           Soda
                           Iced Tea
                           Beer
                       Party Favors
                           Squirt guns
                           Hats
                           Name tags
                       Materials
                           Paper Plates
                           Napkins
                           Trash Containers
               2. Add the check boxes
                  This can be done by visually selecting them and typing 
                  ,,cb.  When done, you should see this:

                   [_] Barbeque
                       [_] Guests
                           [_] Bill and Barb
                           [_] Larry and Louise
                           [_] Marty and Mary
                           [_] Chris and Christine
                           [_] David and Darla
                           [_] Noel and Susan
                       [_] Food
                           [_] Chicken
                           [_] Ribs
                           [_] Corn on the cob
                           [_] Salad
                           [_] Desert
                       [_] Beverages
                           [_] Soda
                           [_] Iced Tea
                           [_] Beer
                       [_] Party Favors
                           [_] Squirt guns
                           [_] Hats
                           [_] Name tags
                       [_] Materials
                           [_] Paper Plates
                           [_] Napkins
                           [_] Trash Containers

               3. Now check off what's done
                  Checking off what is complete is easy with the ,,cx 
                  command.  Just place the cursor on a heading and ,,cx 
                  it. Now you can see what's done as long as the outline 
                  is fully expanded.

                   [_] Barbeque
                       [_] Guests
                           [X] Bill and Barb
                           [X] Larry and Louise
                           [X] Marty and Mary
                           [X] Chris and Christine
                           [X] David and Darla
                           [X] Noel and Susan
                       [_] Food
                           [X] Chicken
                           [X] Ribs
                           [_] Corn on the cob
                           [_] Salad
                           [X] Desert
                       [_] Beverages
                           [_] Soda
                           [X] Iced Tea
                           [X] Beer
                       [_] Party Favors
                           [_] Squirt guns
                           [_] Hats
                           [_] Name tags
                       [_] Materials
                           [X] Paper Plates
                           [_] Napkins
                           [X] Trash Containers

           4. Getting more advanced
               Now summarize what's done.

                You can summarize what is done with the ,,cz command. 
                Place the cursor on the 'Barbeque' heading and ,,cz it. 
                The command will recursively process the outline and 
                update the check boxes of the parent headlines. You 
                should see: 
                   (Note: the only change is on the 'Guests' heading. It 
                   changed because all of its children are complete.)

                   [_] Barbeque
                       [X] Guests
                           [X] Bill and Barb
                           [X] Larry and Louise
                           [X] Marty and Mary
                           [X] Chris and Christine
                           [X] David and Darla
                           [X] Noel and Susan
                       [_] Food
                           [X] Chicken
                           [X] Ribs
                           [_] Corn on the cob
                           [_] Salad
                           [X] Desert
                       [_] Beverages
                           [_] Soda
                           [X] Iced Tea
                           [X] Beer
                       [_] Party Favors
                           [_] Squirt guns
                           [_] Hats
                           [_] Name tags
                       [_] Materials
                           [X] Paper Plates
                           [_] Napkins
                           [X] Trash Containers

               Add percentages for a better view
                   You can get a much better view of what's going on, 
                   especially with collapsed headings, if you add 
                   percentages. Place a % on each heading that has children 
                   like this:

                   [_] % Barbeque
                       [X] % Guests
                           [X] Bill and Barb
                           [X] Larry and Louise
                           [X] Marty and Mary
                           [X] Chris and Christine
                           [X] David and Darla
                           [X] Noel and Susan
                       [_] % Food
                           [X] Chicken
                           [X] Ribs
                           [_] Corn on the cob
                           [_] Salad
                           [X] Desert
                       [_] % Beverages
                           [_] Soda
                           [X] Iced Tea
                           [X] Beer
                       [_] % Party Favors
                           [_] Squirt guns
                           [_] Hats
                           [_] Name tags
                       [_] % Materials
                           [X] Paper Plates
                           [_] Napkins
                           [X] Trash Containers

               Now compute the percentage of completion
                   After adding the % symbols, place the cursor on the 
                   'Barbeque' heading and execute ,,cz as before. Keep in 
                   mind that the recursive percentages are weighted. You 
                   should see:

                   [_] 58% Barbeque
                       [X] 100% Guests
                           [X] Bill and Barb
                           [X] Larry and Louise
                           [X] Marty and Mary
                           [X] Chris and Christine
                           [X] David and Darla
                           [X] Noel and Susan
                       [_] 60% Food
                           [X] Chicken
                           [X] Ribs
                           [_] Corn on the cob
                           [_] Salad
                           [X] Desert
                       [_] 66% Beverages
                           [_] Soda
                           [X] Iced Tea
                           [X] Beer
                       [_] 0% Party Favors
                           [_] Squirt guns
                           [_] Hats
                           [_] Name tags
                       [_] 66% Materials
                           [X] Paper Plates
                           [_] Napkins
                           [X] Trash Containers

               Complete a few more just for fun

                   Mark Salad and Soda and you should see the ouline below.
                  
                   Try plaing around with zc and zo to see the effects of 
                   opening and closing folds. Even if you place the cursor 
                   on 'Barbeque' and zo it, you still have a good 
                   understanding of how complete the project is.

                   [_] 69% Barbeque
                       [X] 100% Guests
                           [X] Bill and Barb
                           [X] Larry and Louise
                           [X] Marty and Mary
                           [X] Chris and Christine
                           [X] David and Darla
                           [X] Noel and Susan
                       [_] 80% Food
                           [X] Chicken
                           [X] Ribs
                           [_] Corn on the cob
                           [X] Salad
                           [X] Desert
                       [X] 100% Beverages
                           [X] Soda
                           [X] Iced Tea
                           [X] Beer
                       [_] 0% Party Favors
                           [_] Squirt guns
                           [_] Hats
                           [_] Name tags
                       [_] 66% Materials
                           [X] Paper Plates
                           [_] Napkins
                           [X] Trash Containers

        Hoisting                                            *vo-hoisting*

            Hoisting is a way to focus on the offspring of the currently
            selected outline item. The subitems will be presented as top
            level items in the automatically extracted hoist-file located
            in the same directory as the main outline file. You cannot
            hoist parts of an already hoisted file again.

            If you installed the add-on, you hoist the subtopics of
            the currently selected item with

            ,,h   Hoist the subtopics into a temporary file

            The changes are merged back into the original file by closing
            the temporary hoist file with

            :q  :wq  :x  ZZ

            If something went wrong, you can perform a manual de-hoisting
            with the following procedure:

            Open the main file in VimOutliner
            Search for the line containing the __hoist tag
            On this line, do

            ,,H    Manual de-hoisting

-------------------------------------------------------------------------
SCRIPTS                                                      *vo-scripts*

    The VimOutliner distribution currently includes two external scripts
    to support interoutline links and HTML export.

    If you want to check out other scripts or experimental stuff,
    take a look at VimOutliner's home page, http://www.vimoutliner.org

        vo_maketags.pl                                      *vo-maketags*

        A basic description of how to use this Perl script is given in
        section |vo-testing|, subsection "Verify interoutline linking".

        otl2html.py                                            *otl2html*

	This Python script transforms an outline into an HTML file. Use
	$ otl2html.py --help to get detailed information.

        This script does not adhere to the VimOutliner naming convention
        with the 'vo_' prefix because it is not necessary for any
        VimOutliner functionality. It is provided both as a useful tool
        for creating HTML pages and HTML slides from outlines and as
        a working demonstration of how to convert .otl files to other
        formats.

-------------------------------------------------------------------------
OTHER INFORMATION                                         *vo-other-info*

    The Vimoutliner Project

        How do I add my own features?
            Two ways -- by changing VimOutliner source code, or by
            inserting your own code in $HOME/.vimoutlinerrc, which runs
            at the end of the VimOutliner startup scripts. You might have
            to merge your personal .vimoutlinerrc with future versions
            to take advantage of new features.

        How is VimOutliner licensed?
            VimOutliner is licensed under the GNU General Public License.

        How do I contribute to VimOutliner
            Step 1 is to subscribe to our mailing list. Join up at
            http://www.lists.vimoutliner.org/. Lurk for a few days or so 
            to get the feel, then submit your idea/suggestion. A lively 
            discussion will ensue, after which your idea, probably in 
            some modified form, will be considered. The more of the actual 
            work you have done, the more likely your feature will go in 
            the distribution in a
            timely manner.

        VimOutliner Naming Convention
            All VimOutliner files must begin with vo_ unless Vim itself
            requires them to have a different name. A few older files
            from previous versions break this rule, but over time these
            will be changed to our naming convention.
            
            In the old days, with the "self contained" philosophy, there
            was no naming convention, because VimOutliner files were
            segregated into their own tree. With the coming of the "vim
            plugin" philosophy, there's a need to identify VimOutliner
            files for purposes of modification, upgrade and
            de-installation. Hence our naming convention.

        What if my feature doesn't make it into the VimOutliner distribution?
            You can offer it Extra-Distro, either on your own website, or
            very possibly on the VimOutliner home page, www.vimoutliner.org.  
            VimOutliner ships with its core features, but many additional 
            functionalities, especially those that operate from Perl scripts 
            (or bash or python) are available outside the distro. For 
            instance, right now there's an Executable Line feature that turns 
            VimOutliner into a true single tree information reservoir. The 
            Executable Line feature is available extra-distro on the 
            VimOutliner home page.

    Anticipated improvements in later versions
        Command-invoking headlines
            Already prototyped
            Probably coming next version
            Allows you to press a key and get an html command in a browser
            Enables a true single tree knowledge collection
            Enables use of VimOutliner as a shell
        Groupware
            Not yet well defined
            Enables collaborative work on an outline
            A pipedream, but VimOutliner itself was once a pipedream
        Easy mode
            Let's Windows users operate VO like a common insert-only
            editor. This will remove a great deal of VO's keyboarder-
            friendly features. But then, they're Windows users: let them
            use the mouse.
        Headline to headline links
            Not yet sanctioned, might never be implemented 
            If implemented, this would presumably create links not just
            between outlines, but between headlines, either in the same
            outline or in a different one. This would be a start on
            "neural networking".
        Headline numbering
            Under feasibility investigation
	    Supported by external scripts
	Toolbar in gvim
            Under feasibility investigation
    Further information on outlines, outline processing and outliners
        http://www.vimoutliner.org
            Main distribution website
        http://www.troubleshooters.com/tpromag/199911/199911.htm
            Outlining discussion, not product specific
        http://www.troubleshooters.com/linux/olvim.htm 
            Discussion on how to use Vim for outlining
        http://www.troubleshooters.com/projects/vimoutliner.htm
            Former Webpage for the VimOutliner distro
        http://www.outliners.com
            Discussion of (proprietary) outliners from days gone by
            Downloads for ancient versions of such outliners
                Unfortunately, all are dos, windows and mac
        http://members.ozemail.com.au/~caveman/Creative/Software/Inspiration/index.html
            Discussion of (proprietary,Mac) Inspiration software
            This page discusses many methods of thought/computer interaction
                Visual Outlining
                Textual Outlining
                Idea mapping
                Mind Mapping
                Brainstorming with Rapid Fire Entry
                Concept Mapping
                Storyboarding
                Diagrams (using rich symbol library)
        http://members.ozemail.com.au/~caveman/Creative/index.html
            Not about outlines, but instead about how to use your brain
            The whole purpose of outlines is to use your brain
            New ways of using your brain produce new ways to use outlines

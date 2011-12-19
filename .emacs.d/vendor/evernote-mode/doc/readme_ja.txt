                         Emacs evernote mode
                         ===================

Author: Yusuke Kawakami <Yusuke Kawakami>
Date: 2011-06-18 09:23:25 JST


Table of Contents
=================
1  License 
2 Introduction 
3 Evernote note edit mode 
    3.1 XHTML モード 
        3.1.1  XHTMLモードでの編集の例: 
    3.2 TEXTモード 
        3.2.1  TEXTモードでの編集の例: 
    3.3 モードの選択 
    3.4 モードの切り替え 
4 Search Query Examples 
5 Evernote Browser 
6 Bookmarks 
7 Install and Settings 
8 Collaboration with Anything 
9 Troubleshooting 
    9.1 `require': no such file to load -- gdbm と表示される 
    9.2 `require': no such file to load -- net/https と表示される 
    9.3 No such file or directory -- enclient.rb (LoadError) の様なメッセージが表示される 


1 License 
~~~~~~~~~~~~~~~~

Copyright 2011 Yusuke Kawakami

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

2 Introduction 
~~~~~~~~~~~~~~~

Emacs evernote modeはEvernoteのノートをemacsから直接参照、編集するための機能を提供します。現在このパッケージでは以下のインターフェースを提供しています。

  - *Command: evernote-login*

    evernote サービスにログインします。以下のコマンドはログイン時にのみ使用可能です。
    ログインしていない状態で以下コマンドを実行した場合はログインプロンプトが表示されます。

  - *Variable: evernote-username*

    evernote サービスのユーザ名を指定します。

  - *Command: evernote-open-note*

    既存のノートをemacsバッファに読み込みます。検索するタグをミニバッファで入力後、続いてノートの名前を入力します。

  - *Command: evernote-open-note-in-notebook*

    既存のノートをemacsバッファに読み込みます。検索するノートブックとタグをミニバッファで入力後、続いてノートの名前を入力します。

  - *Command: evernote-save-note (default bound to \C-x\C-s)*

    編集したノートをEvernoteサービス上で上書き保存します。

  - *Command: evernote-create-note*

    ノートを新規作成します。ノートはデフォルトのノートブックに作成されます。

  - *Command: evernote-create-note-in-notebook*

    ノートを新規作成します。ノートは指定されたノートブックに作成されます。

  - *Command: evernote-write-note*

    emacsバッファを新規ノートとして保存します。ノートはデフォルトのノートブックに作成されます。

  - *Command: evernote-write-note-in-notebook*

    emacsバッファを新規ノートとして保存します。ノートは指定されたノートブックに作成されます。

  - *Command: evernote-post-region*

    選択されたリージョンを新規ノートとしてポストします。ノートはデフォルトのノートブックに作成されます。引数なしで実行した場合は、evernote-open-noteやevernote-create-noteと異なり、新規ノートに対応するバッファは作成しませんが、\C-u等で引数を与えた場合（デフォルト引数以外を与えた場合）は、新規ノートに対応するバッファを作成し、引き続きバッファ上での編集作業を行うことができます。

  - *Command: evernote-post-region-in-notebook*

    選択されたリージョンを新規ノートとしてポストします。ノートは指定されたノートブックに作成されます。

  - *Command: evernote-change-notebook (default bound to \C-cen)*

    ノートが属するノートブックを変更します。このコマンド発行後にevernote-save-noteを実行することでEvernoteサービス上で変更が反映されます.

  - *Command: evernote-edit-tags (default bound to \C-cet)*

    ノートに付加するタグを変更します。このコマンド発行後にevernote-save-noteを実行することでEvernoteサービス上で変更が反映されます.

  - *Command: evernote-change-edit-mode (default bound to \C-cee)*

    ノートの編集モードを変更します.詳細は [Evernote note edit mode] を参照して下さい.このコマンド発行後にevernote-save-noteを実行することでEvernoteサービス上で変更が反映されます.

  - *Command: evernote-rename-note (default bound to \C-cer)*

    ノートを名前を変更します。このコマンド発行後evernote-save-noteを実行することでEvernoteサービス上で変更が反映されます。

  - *Command: evernote-delete-note (default bound to \C-ced)*

    ノートを削除します。

  - *Command: evernote-search-notes*

    ミニバッファから入力されたクエリを使ってノートを検索します。クエリの例は [Search Query Examples] を参照して下さい。

  - *Command: evernote-do-saved-search*

    Evernoteの「保存された検索」を使ってノートを検索します。

  - *Command: evernote-create-notebook*

    ノートブックを新規作成します。

  - *Command: evernote-edit-notebook*

    指定されたノートブックの名前とデフォルトノートブックか否かを設定します。

  - *Command: evernote-create-search*

    クエリに名前をつけて保存し、以後「保存された検索」として参照できるようにします。

  - *Command: evernote-edit-search*

    既存の「保存された検索」の名前とクエリを変更します。

  - *Command: evernote-toggle-read-only (default bound to \C-x\C-q)*

    バッファに読み込まれたノートの読み込み専用，書込み可能状態を切り替えます。XHTMLモードのノートを読み込み専用にした場合、evernote-enml-formatter-command変数に設定されたコマンドを使ってXHTMLをフォーマットして表示します。詳細は [Evernote note edit mode] を参照して下さい

  - *Variable: evernote-enml-formatter-command*

    XHTMLモードのノートを読み込んだ際、もしくは書き込み可能から読み込み専用に状態を切り替えた場合に、XHTMLを整形して表示するためのコマンドを指定します。現在整形コマンドとしてw3mを使用することができます。

  - *Command: evernote-browser*

    Evernote Browser を開きます。Evernote Browser はタグ一覧や、保れされた検索の一覧、過去に検索したノートの一覧からノートを開くための機能を提供します。詳細は[Evernote Browser] を参照して下さい。

  - *Variable: anything-c-source-evernote-title*

    Anything([http://www.emacswiki.org/emacs/Anything]) からタイトルからノートの選択候補を表示する機能を提供する変数です。
    詳細は [Collaboration with Anything] を参照して下さい。

  - *Command: anything-evernote-title*

    Anything を使ってタイトルからノートを開きます。

  - *Variable: evernote-mode-display-menu*

    非 nil の場合に evernote-mode 用のメニューをメニューバー上に表示します。(デフォルト: t)

  - *Variable: evernote-password-cache*

    非 nil の場合にパスワードのキャッシュを有効にします。
    有効にする時は、EasyPG([http://epg.sourceforge.jp/])を使って暗号化することを推奨します。EasyPG は Emacs 23 以降には含まれています。Emacs 22 では EasyPG をインストールし、.emacs に以下を記述してください。
    
    (require 'epa-setup)
    
    gpg-agent を使うことで安全にパスワードをキャッシュできます。

evernote-create-note,evernote-write-note,evernote-post-regionで新規ノートを作成する際にはノートに付加するタグを指定することができます.
また、コマンド使用時にタグ・ノート名を入力する際にはミニバッファでの補完が行われます。

ノートを開いているバッファには evernote-modeマイナーモードが適用されます。以下のコマンドはevernote-modeマイナーモードでのみ有効です。

  - evernote-save-note
  - evernote-change-notebook
  - evernote-edit-tags
  - evernote-change-edit-mode
  - evernote-rename-note
  - evernote-delete-note

3 Evernote note edit mode 
~~~~~~~~~~~~~~~~~~~~~~~~~~

EvernoteのノートはENML DTD([http://xml.evernote.com/pub/enml2.dtd])に準拠するXML文書です。evernote-modeではこのXMLをemacsで保存、読み込みを行う為ににXHTMLモードとTEXTモード2種類の編集モードを用意しています。

3.1 XHTML モード 
=================

XHTMLモードでノートを保存した場合、バッファの内容がそのままノートの内容として保存されます。バッファ内容がENML DTDに沿ったフォーマットでない場合はエラーになります。

XHTMLモードでノートを読み込んだ場合、初期状態としてバッファは読み込み専用になります。この際、変数evernote-enml-formatter-commandが設定されている場合は、バッファには整形された内容が表示されます。(evernote-enml-formatter-comandの設定については [Install and Settings] を参照して下さい) evernote-toggle-read-onlyコマンドを実行して編集の為に書き込み可能にした場合は、整形されない状態のXMLが表示されます。書き込み可能から読み込み専用に再度変更すると、再びバッファには整形された内容が表示されます。


3.1.1 XHTMLモードでの編集の例: 
-------------------------------------

   Emacs バッファ
   -----------------------------------
   <?xml version="1.0" encoding="UTF-8"?>
   <!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
   <en-note>EvernoteのノートはENML DTDに準<br clear="none"/>
   拠するXML文書です。evernote-modeではこのXMLをemacsで扱うためにXHTMLモー<br clear="none"/>
   ドとTEXTモード2種類の編集モードを用意しています。<br clear="none"/>
   </en-note>
   -----------------------------------
   | 
   | XHTMLモードで保存
   V
   Evernoteサービス上のノート(Emacsバッファの内容と同じ)
   -----------------------------------
   <?xml version="1.0" encoding="UTF-8"?>
   <!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
   <en-note>EvernoteのノートはENML DTDに準<br clear="none"/>
   拠するXML文書です。evernote-modeではこのXMLをemacsで扱うためにXHTMLモー<br clear="none"/>
   ドとTEXTモード2種類の編集モードを用意しています。<br clear="none"/>
   </en-note>
   -----------------------------------
   | 
   | XHTMLモードで読み込み
   V
   Emacs バッファ
   (読み込み専用となり、整形されて表示される)
   -----------------------------------
   EvernoteのノートはENML DTDに準
   拠するXML文書です。evernote-modeではこのXMLをemacsで扱うためにXHTMLモー
   ドとTEXTモード2種類の編集モードを用意しています。
   -----------------------------------
   | 
   | 書き込み可能状態にする(evernote-toggle-read-only: \C-x\C-q)
   V
   Emacs バッファ
   (整形されないXMLが表示される)
   -----------------------------------
   <?xml version="1.0" encoding="UTF-8"?>
   <!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
   <en-note>EvernoteのノートはENML DTDに準<br clear="none"/>
   拠するXML文書です。evernote-modeではこのXMLをemacsで扱うためにXHTMLモー<br clear="none"/>
   ドとTEXTモード2種類の編集モードを用意しています。<br clear="none"/>
   </en-note>
   -----------------------------------

3.2 TEXTモード 
===============

XHTMLモードでは、ノートを編集する際にXHTMLをテキストとして編集する必要があり、作業が煩雑になります。そこでevernote-modeではテキストのみ含むEvernoteノートを効率よく作成、編集するための TEXTモードを提供しています。

TEXTモードはテキストのみ含むEvernoteノートの編集に特化したモードです。TEXTモードでノートを保存した場合、バッファ中のXMLの特殊文字(&キーワード\;, スペース、改行)はエスケープされ、ルート要素を付加した上でENMLに変換されます。このため、emacsバッファで表示されている内容がノートの見た目上の内容として保存されます。また、TEXTモードでノートを読み込んだ場合は、XMLのルート要素直下をテキストとして解釈し、XMLの特殊文字はアンエスケープされた上でバッファに読み込まれます。


3.2.1 TEXTモードでの編集の例: 
------------------------------------

   Emacs バッファ
   -----------------------------------
   EvernoteのノートはENML DTDに準
   拠するXML文書です。evernote-modeではこのXMLをemacsで扱うためにXHTMLモー
   ドとTEXTモード2種類の編集モードを用意しています。
   -----------------------------------
   | 
   | TEXTモードで保存
   V
   Evernoteサービス上のノート
   (Emacsバッファの内容がエスケープされ, XMLに変換される)
   -----------------------------------
   <?xml version="1.0" encoding="UTF-8"?>
   <!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
   <en-note>EvernoteのノートはENML DTDに準<br clear="none"/>
   拠するXML文書です。evernote-modeではこのXMLをemacsで扱うためにXHTMLモー<br clear="none"/>
   ドとTEXTモード2種類の編集モードを用意しています。<br clear="none"/>
   </en-note>
   -----------------------------------
   | 
   | TEXTモードで読み込み
   V
   Emacs バッファ
   (ノートのルート要素以下の内容がアンエスケープされる)
   -----------------------------------
   EvernoteのノートはENML DTDに準
   拠するXML文書です。evernote-modeではこのXMLをemacsで扱うためにXHTMLモー
   ドとTEXTモード2種類の編集モードを用意しています。
   -----------------------------------

3.3 モードの選択 
=================

上記XHTML, TEXTモードはノートの作成時に選択できます。ノート保存時には編集モード情報も保存され、次に読み込まれる際には保存時の編集モードで読み込まれます。また、他のEvernoteアプリケーションで作成されたノートはXHTMLモードとして読み込まれます。

3.4 モードの切り替え 
=====================

既存のノートのXHTML,TEXTモードを切り替える場合は、evernote-change-edit-modeコマンドを使用します。XHTMLモードからTEXTモードの切り替えにおいて、バッファが読み込み専用状態の場合、整形された内容がTEXTモードでのノートの内容になります。この際元のXHTMLのフォーマット情報(XML tag)は全て失われるので注意して下さい。書き込み可能状態でモードを切り替えた場合は、整形されていない元のXHTMLがTEXTモード表示されるノートの内容になります。


4 Search Query Examples 
~~~~~~~~~~~~~~~~~~~~~~~~

ノートの検索に使用できるクエリの例を示します。

以下の例は [http://www.evernote.com/about/developer/api/evernote-api.htm#_Toc277181479] からの引用です。

    - 今年に作られたノートで、"chicken"を含み、かつ"cooking"タグが付加されたものを検索します:

      chicken tag:cooking created:year

    - "cooking"タグを含み、かつ"mexican"タグを含まないノートの中で、"beef"を含み"carrots"を含まないものを検索します:

      tag:cooking -tag:mexican beef -carrots

    - "Travel"ノートブック中のノートで、タイトルに"San Francisco"を含むものを検索します:

      notebook:Travel intitle:"San Francisco"

    - "San Francisco"と含むノート、もしくは"SFO"タグを付加されたノートを検索します:

      any: "San Francisco" tag:SFO

5 Evernote Browser 
~~~~~~~~~~~~~~~~~~~

Evernote Browser はタグ一覧や、保存された検索の一覧、過去に検索したノートの一覧からノートを開くための機能を提供します。これらの一覧はevernote-open-noteコマンドやevernote-search-notesコマンドで補完に使われるノート一覧と異なり、ユーザにより削除されるまでバッファ上に保持されます。この一覧は繰り返し使用することができるため、ノートを開く手順を簡略化することができます。

Evernote Browserは複数のEvernote Browserページ(バッファ)から構成されます。Evernote Browserページは、ノートの検索を行った際、もしくは検索を一度も行っていない状態でevernote-browserコマンドを実行した際に作られます。ページはリストとして管理され、現在有効なカレントページと各ページ間に前後関係を持ちます。 Evernote Browser のカレントページに移動するにはevernote-browser コマンドを実行して下さい。また、ページの移動には後述するページ移動キーを使用して下さい。


各 Evernote Browser ページには以下の種類があります。

  - タグ一覧ページ

    タグ一覧ページにはユーザがEvernoteサービス上で作成したタグ一覧が階層的に表示されます。タグ名の上で Enter(\C-m) を押すことで、そのタグが付加されたノートの一覧ページが開きます。

  - 保存された検索一覧ページ

    保存された検索一覧ページにはユーザがEvernoteサービス上で作成した「保存された検索」の一覧が表示されます。保存された検索の名前上でEnter(\C-m)を押すことで保存された検索を実行し、結果をノート一覧ページとして開きます。

  - ノート一覧ページ

    ノート一覧ページは検索により取得したノートの一覧を表示します。ノート一覧ページは evernote-open-note コマンド、 evernote-search-notes コマンドや、Evernote Browser での検索が行われる度に新たに作成されます。ノート名上でEnter(\C-m)を押すことでノートを開きます。

  - ノートブック一覧ページ

    ノートブック一覧ページにはユーザがEvernoteサービス上で作成したノートブックの一覧が表示されます。ノートブック名の上でEnter(\C-m)を押すことで、そのノートブックに属するノートの一覧ページが開きます。


Evernote Browser ページ上でのその他のキーアサインは以下の通りです。

  キー   動作                                                                                                          
 ------+--------------------------------------------------------------------------------------------------------------
  b      前のページに移動します                                                                                        
  f      次のページに移動します                                                                             
  t      タグ一覧ページを作成し、表示します。既にタグ一覧ページがある場合はそのページに移動します                      
  S      保存された検索一覧ページを作成し、表示します。既に保存された検索一覧ページがある場合はそのページに移動します  
  s      入力された検索クエリから結果を新規ノート一覧ページとして作成し、そのページを表示します                        
  N      ノートブック一覧ページを作成し、表示します。既にノートブック一覧ページがある場合はそのページに移動します      
  o      Enter(\C-m) と同じですが、ノート一覧ページの場合は、開いたノートにカーソルを移動しません                      
  n      次の行に移動します。ノート一覧ページの場合は移動したカーソル上のノートを開きます                              
  p      前の行に移動します。ノート一覧ページの場合は移動したカーソル上のノートを開きます                              
  d      現在のページを Evernote Browser から削除します                                                                

6 Bookmarks 
~~~~~~~~~~~~

Emacs 23.1 以降を使用している場合、emacs のブックマークを evernote のノートに対して設定することができます。
この機能により頻繁に参照する evernote ノートをより容易に開くことができます。

ブックマークの使用方法は通常のファイルを扱う場合と同じです。
bookmark-set (C-x r m RET) をノートを開いているバッファで実行することでブックマークを登録します。
また、登録されたブックマークは bookmark-jump (C-x r b bookmark RET) や list-bookmark (C-x r l) で参照できます。

7 Install and Settings 
~~~~~~~~~~~~~~~~~~~~~~~

  1. Evernoteサービス使用のために必要なRubyスクリプトをインストールする

    
    cd evernote-mode/ruby
    ruby setup.rb
    

  2. evernote-mode.el をロードパスにコピーする

    
    cp evernote-mode.el <your load path>
    

  3. evernote-enml-formatter-command に使用するプログラム w3m の入手、設定 (オプション)

     - Linux/Unixの場合、w3m のパッケージを [こちら] から入手してインストールするか、各ディストリビューションのw3mパッケージをインストールして下さい。
     - Windowsの場合、cygwin を [こちら] から入手し、setup.exe を実行してパッケージ選択画面(Select Packages)からw3mを選択してインストールして下さい。
     - w3mが存在するパスを環境変数PATHに追加して下さい

  4. evernote-mode設定を.emacs に追記

     
     (add-to-list 'load-path "<your load path>")
     (require 'evernote-mode)
     (setq evernote-username "<your evernote user name>") ; optional: you can use this username as default.
     (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; optional
     (global-set-key "\C-cec" 'evernote-create-note)
     (global-set-key "\C-ceo" 'evernote-open-note)
     (global-set-key "\C-ces" 'evernote-search-notes)
     (global-set-key "\C-ceS" 'evernote-do-saved-search)
     (global-set-key "\C-cew" 'evernote-write-note)
     (global-set-key "\C-cep" 'evernote-post-region)
     (global-set-key "\C-ceb" 'evernote-browser)
     

     evernote-enml-formatter-commandが設定されていない場合はXHTMLモードでの読み込み時に整形されてない状態のXMLが表示されます。

                 上記に加え、 ruby の cygwin を使用している場合は、cygwin-mount.el ([http://www.emacswiki.org/cgi-bin/wiki/cygwin-mount.el]) が必要になります。cygwin-mount.el を取得し、以下を .emacs に追加して下さい。

     
                 (require 'cygwin-mount)
                 (cygwin-mount-activate)
     

  5. proxy の設定

    プロキシを使用する場合は環境変数EN\_PROXYに 'プロキシホスト':'ポート'を指定して下さい。(ex. export EN\_PROXY=proxy.hoge.com:8080)

8 Collaboration with Anything 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

evernote-mode は Anything([http://www.emacswiki.org/emacs/Anything]) で evernote のノート名から選択候補を表示する機能 (anything-c-source) を提供します。
.emacs に以下の設定を追記することで、anything の選択候補に evernote のノート名を加えます。


(add-to-list 'anything-sources anything-c-source-evernote-title)


また、Anything を使って Evernote のノート名のみから選択を行いたい場合は、 anything-evernote-title を使用できます。

上記 Anything との協調機能は evernote にログインした状態でのみ (evernote-login, もしくは evernote-xxx コマンドを最初に実行した後)
使用することができます。

9 Troubleshooting 
~~~~~~~~~~~~~~~~~~

9.1 `require': no such file to load -- gdbm と表示される 
=========================================================

ディストリビューションの設定によっては ruby が使用できる GDBM ライブラリがインストールされてない場合があります。
上記が表示される場合は libgdbm-ruby 等をインストールして下さい。

- aptを使ったインストール例


apt-get install libgdbm-ruby


- ActiveScriptRuby または Ruby-mswin32 の場合

[Porting Libraries to Win32] から gdbm.dll を入手し、ruby.exe と同じフォルダに置いて下さい。

- Mac OS X の場合

Mac OS X に付属の ruby には GDBM バインディングが含まれていません。
[MacPorts] や [Homebrew] で ruby と GDBM をインストールしてください。

MacPorts の場合

$ sudo ports install ruby gdbm


Homebrew の場合

$ sudo brew install ruby gdbm


また、[evernote-ruby-command を設定] してください。

9.2 `require': no such file to load -- net/https と表示される 
==============================================================

ディストリビューションの設定によっては ruby が使用できる openssl ライブラリがインストールされてない場合があります。
上記が表示される場合は libopenssl-ruby をインストールして下さい。

- aptを使ったインストール例


apt-get install libopenssl-ruby

9.3 No such file or directory -- enclient.rb (LoadError) の様なメッセージが表示される 
======================================================================================

OS に複数のバージョンの ruby がインストールされている場合に、evernote-mode をインストールした ruby (ruby setup.rb を実行した ruby) と異なる ruby が使われている可能性があります。
evernote-mode をロードする前に、evernote-ruby-command に正しい ruby をフルパスで指定してください。

e.g.
    
    (setq evernote-ruby-command "/your/path/to/ruby")
    (require 'evernote-mode)
    

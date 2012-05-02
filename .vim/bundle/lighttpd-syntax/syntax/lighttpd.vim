" Vim syntax file
" Language:     lighttpd config, http://www.lighttpd.net/
" URL:          http://cvs.pld-linux.org/cgi-bin/cvsweb.cgi/packages/vim-syntax-lighttpd/lighttpd.vim
" Version Info: $Revision: 1.16 $
" Maintainer:   Elan Ruusamäe <glen@pld-linux.org>
" Last Change:  $Date: 2009/11/09 12:52:16 $ UTC

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case match

if version < 600
	set iskeyword+=.,-
else
	setlocal iskeyword+=.,-
endif

" define the lighttpd syntax
" see also http://redmine.lighttpd.net/wiki/lighttpd/Docs:Configuration
syn match   lighttpdDelimiter   "[{}()\[\];,]"
syn match   lighttpdOperator    "[~!=|&\*\+\<\>]"
syn match   lighttpdComment     "\(#.*\)"
syn match   lighttpdNumber      "[-+]\=\<\d\+\(\.\d*\)\=\>"
syn region  lighttpdString      start=+"+ skip=+\\"+ end=+"+
syn keyword lighttpdConditional else
syn match   lighttpdServerVar   /\$\(HTTP\|SERVER\|PHYSICAL\)\>/
syn match   lighttpdFunction    /"\(cookie\|host\|useragent\|referer\|url\|querystring\|remoteip\|scheme\|socket\|path\|existing-path\)"/

" internal keywords
syn keyword lighttpdKeyword   include include_shell

syn match   lighttpdSpecial   /"\(dis\|en\)able"/

" module options
syn match   lighttpdOption   /var\.[A-Za-z][_A-Za-z0-9]*/
syn keyword lighttpdOption
	\ accesslog.filename
	\ accesslog.format
	\ accesslog.use-syslog
	\ alias.url
	\ auth.backend
	\ auth.backend.htdigest.userfile
	\ auth.backend.htpasswd.userfile
	\ auth.backend.ldap.allow-empty-pw
	\ auth.backend.ldap.base-dn
	\ auth.backend.ldap.bind-dn
	\ auth.backend.ldap.bind-pw
	\ auth.backend.ldap.ca-file
	\ auth.backend.ldap.filter
	\ auth.backend.ldap.hostname
	\ auth.backend.ldap.starttls
	\ auth.backend.plain.groupfile
	\ auth.backend.plain.userfile
	\ auth.debug
	\ auth.require
	\ cgi.assign
	\ cgi.execute-x-only
	\ cml.extension
	\ cml.memcache-hosts
	\ cml.memcache-namespace
	\ cml.power-magnet
	\ compress.allowed-encodings
	\ compress.cache-dir
	\ compress.filetype
	\ compress.max-filesize
	\ connection.kbytes-per-second
	\ debug.log-condition-handling
	\ debug.log-file-not-found
	\ debug.log-request-handling
	\ debug.log-request-header
	\ debug.log-request-header-on-error
	\ debug.log-response-header
	\ debug.log-ssl-noise
	\ debug.log-state-handling
	\ debug.log-timeouts
	\ dir-listing.activate
	\ dir-listing.auto-layout
	\ dir-listing.encode-header
	\ dir-listing.encode-readme
	\ dir-listing.encoding
	\ dir-listing.exclude
	\ dir-listing.external-css
	\ dir-listing.hide-dotfiles
	\ dir-listing.hide-header-file
	\ dir-listing.hide-readme-file
	\ dir-listing.set-footer
	\ dir-listing.show-header
	\ dir-listing.show-readme
	\ etag.use-inode
	\ etag.use-mtime
	\ etag.use-size
	\ evasive.http-status-code
	\ evasive.max-conns-per-ip
	\ evasive.retry-after
	\ evasive.silent
	\ evhost.path-pattern
	\ expire.url
	\ extforward.forwarder
	\ extforward.headers
	\ fastcgi.debug
	\ fastcgi.map-extensions
	\ fastcgi.server
	\ flv-streaming.extensions
	\ h264-streaming.extensions
	\ index-file.names
	\ mimetype.assign
	\ mimetype.use-xattr
	\ proxy.balance
	\ proxy.debug
	\ proxy.server
	\ rrdtool.binary
	\ rrdtool.db-name
	\ scgi.debug
	\ scgi.server
	\ secdownload.document-root
	\ secdownload.secret
	\ secdownload.timeout
	\ secdownload.uri-prefix
	\ server.bind
	\ server.breakagelog
	\ server.chroot
	\ server.core-files
	\ server.defer-accept
	\ server.dir-listing
	\ server.document-root
	\ server.errorfile-prefix
	\ server.error-handler-404
	\ server.errorlog
	\ server.errorlog-use-syslog
	\ server.event-handler
	\ server.follow-symlink
	\ server.force-lowercase-filenames
	\ server.groupname
	\ server.indexfiles
	\ server.kbytes-per-second
	\ server.max-connections
	\ server.max-fds
	\ server.max-keep-alive-idle
	\ server.max-keep-alive-requests
	\ server.max-read-idle
	\ server.max-request-size
	\ server.max-worker
	\ server.max-write-idle
	\ server.modules
	\ server.name
	\ server.network-backend
	\ server.pid-file
	\ server.port
	\ server.protocol-http11
	\ server.range-requests
	\ server.reject-expect-100-with-417
	\ server.stat-cache-engine
	\ server.tag
	\ server.upload-dirs
	\ server.use-ipv6
	\ server.username
	\ setenv.add-environment
	\ setenv.add-request-header
	\ setenv.add-response-header
	\ simple-vhost.debug
	\ simple-vhost.default-host
	\ simple-vhost.document-root
	\ simple-vhost.server-root
	\ ssi.content-type
	\ ssi.extension
	\ ssl.ca-file
	\ ssl.cipher-list
	\ ssl.engine
	\ ssl.pemfile
	\ ssl.use-sslv2
	\ ssl.verifyclient.activate
	\ ssl.verifyclient.depth
	\ ssl.verifyclient.enforce
	\ ssl.verifyclient.exportcert
	\ ssl.verifyclient.username
	\ static-file.etags
	\ static-file.exclude-extensions
	\ status.config-url
	\ status.enable-sort
	\ status.statistics-url
	\ status.status-url
	\ trigger-before-download.debug
	\ trigger-before-download.deny-url
	\ trigger-before-download.download-url
	\ trigger-before-download.gdbm-filename
	\ trigger-before-download.memcache-hosts
	\ trigger-before-download.memcache-namespace
	\ trigger-before-download.trigger-timeout
	\ trigger-before-download.trigger-url
	\ url.access-deny
	\ url.redirect
	\ url.rewrite
	\ url.rewrite-final
	\ url.rewrite-if-not-file
	\ url.rewrite-once
	\ url.rewrite-repeat
	\ url.rewrite-repeat-if-not-file
	\ userdir.basepath
	\ userdir.exclude-user
	\ userdir.include-user
	\ userdir.letterhomes
	\ userdir.path
	\ usertrack.cookie-domain
	\ usertrack.cookie-max-age
	\ usertrack.cookie-name
	\ webdav.activate
	\ webdav.is-readonly
	\ webdav.log-xml
	\ webdav.sqlite-db-name

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_lighttpd_syntax_inits")
  if version < 508
    let did_lighttpd_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink lighttpdDelimiter   Delimiter
  HiLink lighttpdOperator    Operator
  HiLink lighttpdComment     Comment
  HiLink lighttpdNumber      Number
  HiLink lighttpdFunction    Function
  HiLink lighttpdKeyword     Keyword
  HiLink lighttpdOption      Identifier
  HiLink lighttpdSpecial     Special
  HiLink lighttpdConditional Conditional
  HiLink lighttpdString      String
  HiLink lighttpdServerVar   Identifier

  delcommand HiLink
endif

let b:current_syntax = "lighttpd"

" vim: ts=4

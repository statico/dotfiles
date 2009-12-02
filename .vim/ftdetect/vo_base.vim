"# #######################################################################
"# filetype.vim: filetype loader
"#   VimOutliner functions, commands and settings, version 0.2.0
"#   Copyright (C) 2001,2003 by Steve Litt (slitt@troubleshooters.com)
"#
"#   This program is free software; you can redistribute it and/or modify
"#   it under the terms of the GNU General Public License as published by
"#   the Free Software Foundation; either version 2 of the License, or
"#   (at your option) any later version.
"#
"#   This program is distributed in the hope that it will be useful,
"#   but WITHOUT ANY WARRANTY; without even the implied warranty of
"#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"#   GNU General Public License for more details.
"#
"#   You should have received a copy of the GNU General Public License
"#   along with this program; if not, write to the Free Software
"#   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
"#
"# Steve Litt, slitt@troubleshooters.com, http://www.troubleshooters.com
"# #######################################################################

"# #######################################################################
"#  HISTORY
"#  V0.1.0 Pre-alpha
"#      Set of outliner friendly settings
"# Steve Litt, 5/28/2001
"# End of version 0.1.0
"# 
"#  V0.1.1 Pre-alpha
"#      No change
"# 
"# Steve Litt, 5/28/2001
"# End of version 0.1.1
"# 
"#  V0.1.2 Pre-alpha
"# 	No Change
"# Steve Litt, 5/30/2001
"# End of version 0.1.2
"#  V0.1.3 Pre-alpha
"# 	No Change
"# Steve Litt, 5/30/2001
"# End of version 0.1.3
"#  V0.2.0 
"# 	Noel Henson adds code for outliner-friendly expand and
"# 	collapse, comma comma commands, color coding, hooks for a
"# 	spellchecker, sorting, and date insertion.
"# Noel Henson, 11/24/2002
"# End of version 0.2.0
"# END OF HISTORY
"# 
"# #######################################################################
"filetype.vim
"http://www.troubleshooters.com/projects/vimoutliner

"Internal RCS
"$Revision: 1.1 $"
"$Date: 2005/01/19 16:12:37 $
"$Log: vo_base.vim,v $
"Revision 1.1  2005/01/19 16:12:37  noel
"Finally added to CVS.
"Don't know how I missed this one.
"
"Revision 1.4  2003/03/01 17:37:17  noel
"Changed the filetype name to our new standard: vo_base
"
"Revision 1.3  2003/02/14 17:49:43  noel
"Removed unnecessary filetype commands
"
"Revision 1.2  2003/02/09 15:07:35  noel
"Changed the auto commands. Setting the "filetype plugin indent on"
"option here does not work with ftplugins.
"
"Revision 1.1  2003/02/08 21:11:26  noel
"Initial revision
"
if exists("did_load_filetypes")
  finish
endif
augroup filetypedetect
  au! BufRead,BufNewFile *.otl		setfiletype vo_base
  au! BufRead,BufNewFile *.oln		setfiletype xoutliner
augroup END


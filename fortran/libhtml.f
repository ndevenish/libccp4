C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C     November 1998 KDC
C     WROTE IT!
C
C    =========
C    libhtml.f
C    =========
C
C Link destinations can be of the for "#name" for a link in the same
C document. Any other link will be prepended with $CHTML to link to
C the documentation area
C
C The contents of the environment variable $CCP_PROGRAM_ID is concatenated
c to the end of each link name. This allows multiple runs of a single 
C program from a script to be combined in one file without the links
C conflicting.
C
C The environment variable $CCP_SUPPRESS_HTML if set suppresses the output
C of most HTML tags. This is useful for devotees of plain text, and
C especially for the command input section.
C
C   ccp4h_init() - write initial comment to identify file
C
C   ccp4h_toc_beg() - write starting Contents section tags
C
C   ccp4h_toc_ent(char*(*) text, char*(*) dest) - write Contents entry
C     text= the link text
C     dest= the link destination
C
C   ccp4h_toc_end() - write ending Contents section tags
C
C   ccp4h_graph_beg(x,y) - write starting JLogGraph applet tag
C     x = width of graph in pixels
C     y = height of graph in pixels
C      both can be zero, then they default to 400,300.
C
C   ccp4h_graph_end() - write ending JLogGraph applet tag
C
C   ccp4h_pre_beg() - begin preformatted (html <pre> tag)
C
C   ccp4h_pre_end() - end preformatted (html <pre> tag)
C
C   ccp4h_rule() - rule (html <hr> tag)
C
C   ccp4h_link(char*(*) text, char*(*) dest) - link (html <a> tag)
C     text= the link text
C     dest= the link destination
C       if dest is not an anchor name (i.e. begins with '#'), then
C       the $CHTML path is prefixed automatically.
C
C   ccp4h_link_key(char*(*) text, char*(*) dest) - link to CCP4 documentation
C     text= the keyparser keyword
C     dest= the link destination filename and anchor,
C       the $CHTML path is prefixed automatically.
C     Between call memparse(.false.) and the main keyparser processing,
C     call this routine with any keywords and links which should be
C     linked to the program doc. Then call parsefail() to restart
C     syntax checking.... e.g.
C      call memoparse(.false)
C      call ccp4h_link_key('LABIN   ','fft.html#labin')
C      call ccp4h_link_key('GRID    ','fft.html#grid')
C      call ccp4h_link_key('FFTSPGRP','fft.html#fftspgrp')
C      call parsefail()
C
C   ccp4h_header(char*(*) text, char*(*) name, int level) - header
C     text= the link text
C     name= header name to link to
C     level=0-6. Header size. 0= plain text
C
      subroutine ccp4h_init()
C   ccp4h_init() - write initial comment to identify file
      integer lpt, idum
      logical html
      character cbin*160,chtml*160,cpid*160,dummy*160
      common /ccp4hdat/lpt,html,cbin,chtml,cpid
      save   /ccp4hdat/
      lpt=lunsto(idum)
      call ugtenv('CBIN',cbin)
      call ugtenv('CHTML',chtml)
      call ugtenv('CCP_PROGRAM_ID',cpid)
      call ugtenv('CCP_SUPPRESS_HTML',dummy)
      html=(dummy.eq.' ')
      if (html) write (lpt,10)
 10   format('<html> <!-- CCP4 HTML LOGFILE -->')
      return
      end
C
      subroutine ccp4h_toc_beg()
C   ccp4h_toc_beg() - write starting Contents section tags
      integer lpt
      logical html
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,html,cbin,chtml,cpid
      save   /ccp4hdat/
      call ccp4h_header('Contents','toc',2)
      if (html) write (lpt,10)
 10   format('<ul>')
      return
      end
C
      subroutine ccp4h_toc_ent(text,dest)
C   ccp4h_toc_ent(char*(*) text, char*(*) dest) - write Contents entry
C     text= the link text
C     dest= the link destination
      character*(*) text,dest
      integer lpt
      logical html
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,html,cbin,chtml,cpid
      save   /ccp4hdat/
      integer lenstr
      external lenstr
      if (html) then
       write (lpt,10)dest,cpid(1:lenstr(cpid)),text
      else
       write (lpt,11)text
      endif
 10   format('<li><a href="',a,a,'">',a,'</a>')
 11   format(a)
      return
      end
C
      subroutine ccp4h_toc_end()
C   ccp4h_toc_end() - write ending Contents section tags
      integer lpt
      logical html
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,html,cbin,chtml,cpid
      save   /ccp4hdat/
      if (html) write (lpt,10)
 10   format('</ul>')
      return
      end
C
      subroutine ccp4h_graph_beg(x,y)
C   ccp4h_graph_beg() - write starting JLogGraph applet tag
C     x = width of graph in pixels
C     y = height of graph in pixels
C      both can be zero, then they default to 400,300.
      integer x,y,x1,y1,lenstr
      integer lpt
      logical html
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,html,cbin,chtml,cpid
      save   /ccp4hdat/
      external lenstr
      x1=x
      y1=y
      if (x1.le.0) x1=400
      if (y1.le.0) y1=300
      if (html) write (lpt,20)x1,y1,cbin(1:lenstr(cbin))
 20   format(
     +  '<applet width="',i4,'" height="',i4,'" code="JLogGraph.class" '
     +  ,/,'codebase="',a,'"><param name="table" value="')
      return
      end
C
      subroutine ccp4h_graph_end()
C   ccp4h_graph_end() - write ending JLogGraph applet tag
      integer lpt
      logical html
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,html,cbin,chtml,cpid
      save   /ccp4hdat/
      if (html) write (lpt,10)
 10   format('"><b>For inline graphs use a Java browser</b></applet>')
      return
      end
C
      subroutine ccp4h_pre_beg()
C   ccp4h_pre_beg() - begin preformatted (html <pre> tag)
      integer lpt
      logical html
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,html,cbin,chtml,cpid
      save   /ccp4hdat/
      if (html) write (lpt,10)
 10   format('<pre>')
      return
      end
C
      subroutine ccp4h_pre_end()
C   ccp4h_pre_end() - end preformatted (html <pre> tag)
      integer lpt
      logical html
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,html,cbin,chtml,cpid
      save   /ccp4hdat/
      if (html) write (lpt,10)
 10   format('</pre>')
      return
      end
C
      subroutine ccp4h_rule()
C   ccp4h_rule() - rule (html <hr> tag)
      integer lpt
      logical html
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,html,cbin,chtml,cpid
      save   /ccp4hdat/
      if (html) then
       write (lpt,10)
 10    format('<hr>')
      else
       write (lpt,11)
 11    format('-------------------------------------------------------')
      endif
      return
      end
C
      subroutine ccp4h_link(text,dest)
C   ccp4h_link(char*(*) text, char*(*) dest) - link (html <a> tag)
C     text= the link text
C     dest= the link destination
      character*(*) text,dest
      integer lpt
      logical html
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,html,cbin,chtml,cpid
      save   /ccp4hdat/
      integer lenstr
      external lenstr
      if (html) then
       if (dest(1:1).eq.'#') then
        write (lpt,10)dest,cpid(1:lenstr(cpid)),text
 10     format('<a href="',a,a,'">',a,'</a>')
       else
        write (lpt,20)chtml(1:lenstr(chtml)),dest,text
 20     format('<a href="',a,'/',a,'">',a,'</a>')
       endif
      else
       write (lpt,30)text
 30    format(a)
      endif
      return
      end
C
      subroutine ccp4h_link_key(key,dest)
C   ccp4h_link(char*(*) text, char*(*) dest) - link keyword
C     text= the link text
C     dest= the link destination
      character*(*) key,dest
      integer lpt
      logical html
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,html,cbin,chtml,cpid
      save   /ccp4hdat/
      character kw*4,rest*120
      logical flag
      integer lenstr
      external lenstr
      flag=.false.
      kw=key
      call parsesubkey(kw,'    ',flag)
      if (flag) then
       call parsekeyarg(kw,rest)
       if (html) then
        write(lpt,20)
     +    chtml(1:lenstr(chtml)),dest,key,rest(1:lenstr(rest))
 20     format('<a href="',a,'/',a,'">',a,'</a> ',a)
       else
        write(lpt,30)key,rest(1:lenstr(rest))
 30     format(a,a)
       endif
      endif
      return
      end
C
      subroutine ccp4h_header(text,name,level)
C   ccp4h_header(char*(*) text, char*(*) name, int level) - header
C     text= the link text
C     name= header name to link to
C     level=0-6. Header size. 0= plain text
      character*(*) text,name
      integer level,lenstr
      integer lpt
      logical html
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,html,cbin,chtml,cpid
      save   /ccp4hdat/
      character*60 underline
      external lenstr
      if (html) then
       if (level.gt.0) then
        write (lpt,10)name,cpid(1:lenstr(cpid)),level,text,level
 10     format(/,'<a name="',a,a,'"><h',i1,'>',a,'</h',i1,'></a>')
       else
        write (lpt,20)name,cpid(1:lenstr(cpid)),text
 20     format(/,'<a name="',a,a,'">',a,'</a>')
       endif
      else
       if (level.le.0) then
        write(lpt,30)text(1:lenstr(text))
 30     format(a)
       else if (level.eq.1.or.level.eq.2.or.level.eq.3) then
        underline=
     +    '------------------------------------------------------------'
        write(lpt,31)text(1:lenstr(text)),
     +               underline(1:min(lenstr(text),60))
 31     format(/,/,a,/,a,/)
       else
        write(lpt,32)text(1:lenstr(text))
 32     format(/,a,/)
       endif
      endif
      return
      end
C

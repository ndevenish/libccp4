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
C The contents of the environment variable $CPID is concatenated to the
C end of each link name. This allows multiple runs of a single program
C from a script to be combined in one file without the links conflicting.
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
      integer lpt
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,cbin,chtml,cpid
      save   /ccp4hdat/
      lpt=lunsto()
      write (lpt,10)
 10   format('<!-- CCP4 HTML LOGFILE -->')
      call ugtenv('CBIN',cbin)
      call ugtenv('CHTML',chtml)
      call ugtenv('CCP_PROGRAM_ID',cpid)
      return
      end
C
      subroutine ccp4h_toc_beg()
C   ccp4h_toc_beg() - write starting Contents section tags
      integer lpt
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,cbin,chtml,cpid
      save   /ccp4hdat/
      call ccp4h_header('Contents','toc',2)
      write (lpt,10)
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
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,cbin,chtml,cpid
      save   /ccp4hdat/
      integer lenstr
      external lenstr
      write (lpt,10)dest,cpid(1:lenstr(cpid)),text
 10   format('<li><a href="',a,a,'">',a,'</a>')
      return
      end
C
      subroutine ccp4h_toc_end()
C   ccp4h_toc_end() - write ending Contents section tags
      integer lpt
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,cbin,chtml,cpid
      save   /ccp4hdat/
      write (lpt,10)
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
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,cbin,chtml,cpid
      save   /ccp4hdat/
      external lenstr
      x1=x
      y1=y
      if (x1.le.0) x1=400
      if (y1.le.0) y1=300
      write (lpt,20)x1,y1,cbin(1:lenstr(cbin))
 20   format(
     +  '<applet width="',i4,'" height="',i4,'" code="JLogGraph.class" '
     +  ,/,'codebase="',a,'"><param name="table" value="')
      return
      end
C
      subroutine ccp4h_graph_end()
C   ccp4h_graph_end() - write ending JLogGraph applet tag
      integer lpt
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,cbin,chtml,cpid
      save   /ccp4hdat/
      write (lpt,10)
 10   format('"><b>For inline graphs use a Java browser</b></applet>')
      return
      end
C
      subroutine ccp4h_pre_beg()
C   ccp4h_pre_beg() - begin preformatted (html <pre> tag)
      integer lpt
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,cbin,chtml,cpid
      save   /ccp4hdat/
      write (lpt,10)
 10   format('<pre>')
      return
      end
C
      subroutine ccp4h_pre_end()
C   ccp4h_pre_end() - end preformatted (html <pre> tag)
      integer lpt
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,cbin,chtml,cpid
      save   /ccp4hdat/
      write (lpt,10)
 10   format('</pre>')
      return
      end
C
      subroutine ccp4h_rule()
C   ccp4h_rule() - rule (html <hr> tag)
      integer lpt
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,cbin,chtml,cpid
      save   /ccp4hdat/
      write (lpt,10)
 10   format('<hr>')
      return
      end
C
      subroutine ccp4h_link(text,dest)
C   ccp4h_link(char*(*) text, char*(*) dest) - link (html <a> tag)
C     text= the link text
C     dest= the link destination
      character*(*) text,dest
      integer lpt
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,cbin,chtml,cpid
      save   /ccp4hdat/
      integer lenstr
      external lenstr
      if (dest(1:1).eq.'#') then
       write (lpt,10)dest,cpid(1:lenstr(cpid)),text
 10    format('<a href="',a,a,'">',a,'</a>')
      else
       write (lpt,20)chtml(1:lenstr(chtml)),dest,text
 20    format('<a href="',a,a,'">',a,'</a>')
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
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,cbin,chtml,cpid
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
       write(lpt,20)chtml(1:lenstr(chtml)),dest,key,rest(1:lenstr(rest))
 20    format('<a href="',a,'/',a,'">',a,'</a> ',a)
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
      character cbin*160,chtml*160,cpid*160
      common /ccp4hdat/lpt,cbin,chtml,cpid
      save   /ccp4hdat/
      external lenstr
      if (level.gt.0) then
       write (lpt,10)name,cpid(1:lenstr(cpid)),level,text,level
 10    format(/,'<a name="',a,a,'"><h',i1,'>',a,'</h',i1,'></a>')
      else
       write (lpt,20)name,cpid(1:lenstr(cpid)),text
 20    format(/,'<a name="',a,a,'">',a,'</a>')
      endif
      return
      end
C





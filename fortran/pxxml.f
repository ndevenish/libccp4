C
C     pxxml.f: write XML tags into program outout
C     Copyright (C) 2001  Alun Ashton
C
C     This code is distributed under the terms and conditions of the
C     CCP4 Program Suite Licence Agreement as a CCP4 Library.
C     A copy of the CCP4 licence can be obtained by writing to the
C     CCP4 Secretary, Daresbury Laboratory, Warrington WA4 4AD, UK.
C
C
C
C    =========
C    pxxml.f
C    =========
C
C     Author  : a.w.ashton@ccp4.ac.uk
C     Version : 1.0  
C     future  : translate to C or C++
C  
C    Assumption 1 : Exsisting/New file
C     XML files are only to be writen from new. The design must take into consideration 
C     the possability of future extension so that XML file might be reopned. 
C
C    Assumption 2 : Sizes
C     The maximum size for an Element value or name is 80 characters.
C     The maximum size for an Attribute value or name is 80 characters. 
C     The maximum size for program name is 20 characters.
C     The maximum size for XML filename – including path is 70 characters.
C     The maximum number of elements is 80.
C
C    Assumption 3: Language
C     In the first instance the libs will be writen in fortran to test the other 
C     assumptions!

C     SUBROUTINE XMLOpen (XMLFileUnit, rootvalue, ifail)
C
C     First subroutine to be called.
C     Arguments: 	XMLFIleUnit : Input - No value
C                           Output returns the file Unit number
C               root value = eg html
C		Ifail : same as usual!
C
C     Purpose: open an XML file.
C
C     See if XMLFILE has been assigned e.g. on command line.
C     If XMLFILE not assigned use program name.xml.
C     If file exists overwrite it.
C     Write out the header.
C     No provision for existing XML files to be reopened

      subroutine XMLOpen (XMLFileUnit, rootvalue, ifail)

      logical ccpexs
      
      character*(*) rootvalue

      integer XMLFileUnit, ifail, elementstatus(80), elementnumber 
      character*80 openelements(80), XMLFileName

      character version*10, date*8, progname*20
      
      common /ccp4xmld/ elementnumber, elementstatus, openelements, 
     $     XMLFileName

      integer lenstr
      external ccpexs,lenstr

      XMLFileName=' '
      elementnumber=0
      elementstatus(1)=0

      call ccp4_version(version)
      call ccpdat(date)
      call ccppnm(progname)

      call ugtenv('XMLFILE',XMLFileName)

      if (XMLFileUnit.ne.6) then
        if (XMLFileName.eq.' ') 
     +       XMLFileName=progname(1:lenstr(progname))//'.xml'
        
        if (ccpexs(XMLFileName)) call ccperr(2,'XMLFILE OVERWRITTEN')
        
        call ccpdpn(XMLFileUnit,XMLFileName,'UNKNOWN','F',0,ifail)
      endif

C     check to see if the rootvalue has a value or default to html
      if (lenstr(rootvalue).le.1) rootvalue='html'

      if (rootvalue(1:lenstr(rootvalue)).ne.'html') 
     $     write (XMLFileUnit,'(A)') '<?xml version="1.0"?>'

      call XMLOpenElement(XMLFileUnit, rootvalue(1:lenstr(rootvalue)), 
     $     ifail) 

      if (rootvalue(1:lenstr(rootvalue)).ne.'html') then
C     First element should be information about the program
        call XMLOpenElement(XMLFileUnit, 
     $       progname(1:lenstr(progname)), ifail)
        call XMLWriteAttribute(XMLFileUnit, 
     $       progname(1:lenstr(progname)),
     $       'ccp4_version', version(1:lenstr(version)), ifail)
        call XMLWriteAttribute(XMLFileUnit, 
     $       progname(1:lenstr(progname)),
     $   'date', date(1:lenstr(date)), ifail)
        call XMLCloseElement(XMLFileUnit, progname(1:lenstr(progname)), 
     $       ifail)
      endif

      return
      end

C subroutine XMLOpenElement(XMLFileUnit, ElementName, ifail)
C     XMLFileUnit - integer - if file not already opened it will be
C     element name = value is char string.
C        completes a tag and reates a sub element but leaves tag open
C        e.g. <oldtag
C             >
C              <leftopen

      subroutine XMLOpenElement(XMLFileUnit, ElementName, ifail)

      logical ccpexs

      character*(*) ElementName

      character*80 indentline
      integer indent

      integer XMLFileUnit, ifail, elementstatus(80), elementnumber 
      character*80 openelements(80), XMLFileName
      
      common /ccp4xmld/ elementnumber, elementstatus, openelements,
     $     XMLFileName
c      save /ccp4xml/

      integer lenstr
      external ccpexs,lenstr

      indentline=' ' 

      if (.not.ccpexs(XMLFileName) .and. .not.(XMLFileUnit.eq.6)) then
        call XMLOpen (XMLFileUnit, ElementName, ifail)
        elementnumber=elementnumber-1
      endif

      elementnumber=elementnumber+1

      openelements(elementnumber)=ElementName(1:lenstr(ElementName))

      if (elementnumber.eq.1) then
        if (elementstatus(1).ne.2) then       
          write (XMLFileUnit, 95) ElementName(1:lenstr(ElementName))
          elementstatus(elementnumber) = 2
        endif
        return
      endif

c     close previous element if needed
      if (elementnumber.gt.1) then
        if (elementstatus(elementnumber-1).eq.1) then
c     indent the closing tag
          do 5 indent = 1, elementnumber-1
            write (indentline(indent:indent), 110)
 5        continue
c     add the closing bracket and print
          write (indentline(elementnumber:elementnumber),100)
          write (XMLFileUnit,120) indentline(1:elementnumber)
c     change status to complete and open
          elementstatus(elementnumber-1)=2
        endif
      endif
c
c     open current element
c     firstly indent line
      do 10 indent = 1, elementnumber
        write (indentline(indent:indent), 110)
 10   continue
c     write indent and tag
      write (XMLFileUnit,130) indentline(1:elementnumber),
     $     ElementName(1:lenstr(ElementName))

      elementstatus(elementnumber)=1

c unindented tag
 95   format(' <',a,'>')
c tag end and start...
 100  format('>')
 105  format('<')
c this is the indent
 110  format(' ')
c simply a string...
 120  format(a)
c indented open tag
 130  format(a,'<',a)
      return
      end

C     subroutine XMLWriteAttribute 
C     ElementName - if element not already opened then it will be
C     these are strings: AttributeName, AttributeValue,
C
      subroutine XMLWriteAttribute(XMLFileUnit, ElementName,
     $     AttributeName, AttributeValue, ifail)

      integer lenstr

      character*(*) ElementName, AttributeName, AttributeValue

      character*80 indentline
      integer indent

      integer XMLFileUnit, ifail, elementstatus(80), elementnumber 
      character*80 openelements(80), XMLFileName
      
      common /ccp4xmld/ elementnumber, elementstatus, openelements,
     $     XMLFileName
c      save /ccp4xml/

      external lenstr

      indentline=' ' 

c     firstly check element is open otherwise open!
      if (elementstatus(elementnumber).ne.1) then
        call XMLOpenElement(XMLFileUnit, ElementName, ifail)
      endif 
c
c     firstly indent line
      do 10 indent = 1, elementnumber
        write (indentline(indent:indent), 110)
 10   continue

c     secondly write the attribute 
      write (XMLFileUnit, 120) indentline(1:elementnumber),
     $     AttributeName(1:lenstr(AttributeName)),
     $     AttributeValue(1:lenstr(AttributeValue))


c this is the indent
 110  format(' ')
c simply a string...
 120  format(a,'  ',a,'="',a,'" ')

      return
      end

C     subroutine XMLWriteElement
C     for inputing the value of the element. if its not already opened
C     it will be. Closes the element as well
C
      subroutine XMLWriteElement(XMLFileUnit, ElementName,
     $     ElementValue, ifail)
      
      character*(*) ElementName, ElementValue

      character*80 indentline
      integer indent

      integer XMLFileUnit, ifail, elementstatus(80), elementnumber 
      character*80 openelements(80), XMLFileName
      
      common /ccp4xmld/ elementnumber, elementstatus, openelements,
     $     XMLFileName
c      save /ccp4xml/

      integer lenstr
      external lenstr

      indentline=' '
      indent=0

      if (ElementName(1:lenstr(ElementName)).ne.
     $     openelements(elementnumber)) then
        call XMLOpenElement(XMLFileUnit, ElementName, ifail)
      endif

      if (elementnumber.ne.1) then
c     complete element and give the value
        do 10 indent = 1, elementnumber
          write (indentline(indent:indent), 110)
 10     continue
        write (XMLFileUnit, 135) indentline(1:elementnumber)
        write (XMLFileUnit, 125) indentline(1:elementnumber), 
     $       ElementValue(1:lenstr(ElementValue))
        elementstatus(elementnumber)=2
      endif

      call XMLCloseElement(XMLFileUnit, ElementName, ifail)
      

c unindented root tag
 95   format('<',a,'>')
c tag end and start...
 100  format('>')
 105  format('<')
c this is the indent
 110  format(' ')
c simply a string...
 120  format(a)
c an indented string
 125  format(a,a)
c indented open tag
 130  format(a,'<',a)
c indented completion tag
 135  format(a, '>')
c indented complete tag
 140  format(a,'<',a,'>')
c indented complete end tag
 150  format(a,'</',a,'>')

      return
      end

c subroutine XMLCloseElement
C the subroutine will close the element if it is the current element
C or go up the tree closing all elements till it reaches teh one specified.
C if its the root element being specified then it will close the file as well.
C
      subroutine XMLCloseElement(XMLFileUnit, ElementName, ifail)

      character*(*) ElementName

      character*80 indentline
      integer indent, elementlength

      integer XMLFileUnit, ifail, elementstatus(80), elementnumber 
      character*80 openelements(80), XMLFileName
      
      integer lenstr
      external lenstr

      common /ccp4xmld/ elementnumber, elementstatus, openelements,
     $     XMLFileName
c      save /ccp4xml/

 1    continue

      elementlength=lenstr(openelements(elementnumber))

      indentline=' '
      indent=0

      do 10 indent = 1, elementnumber
        write (indentline(indent:indent), 110)
 10   continue

      if (ElementName(1:lenstr(ElementName)).ne.
     $     openelements(elementnumber)) then
C     if the element to close is not the open one then close 
C     the open one - can assume its just e.g. <wibble and not
C     <wibble> so only need a />
        write (XMLFileUnit, 155) indentline(1:elementnumber)
        openelements(elementnumber)=' '
        elementstatus(elementnumber)=0
        elementnumber=elementnumber-1
        goto 1
      else
        if (elementstatus(elementnumber).eq.1) then
          write (XMLFileUnit, 155) indentline(1:elementnumber)
          elementstatus(elementnumber)=2
        else
          write (XMLFileUnit, 150) indentline(1:elementnumber),
     $         openelements(elementnumber)(1:elementlength)
        endif
        openelements(elementnumber)=' '
        elementstatus(elementnumber)=0
        elementnumber=elementnumber-1
      endif

      if (elementnumber.eq.0) then
        close (XMLFileUnit)
      endif

c this is the indent
 110  format(' ')
c indented completion tag
 135  format(a, '>')
c indented complete end tag
 150  format(a,'</',a,'>')
c indented complete end tag
 155  format(a,' />')

      return
      end 







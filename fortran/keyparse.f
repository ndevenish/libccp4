C
C     A simplified interface to the %$!$#@$ (so-called) parser:
C
C     The idea is to call a routine (MEMOPARSE) to read a keyworded line
C     into a hidden internal buffer and then have a simple chain of
C     calls to a set of routines which check the appropriate keyword,
C     look for any extra arguments associated with it, and set values in
C     their arguments as appropriate.  No if ... then ... else (or, much
C     worse, assigned goto) is necessary, nor maintaining the parser
C     arrays -- the relevant checking is done internally.  At the end of
C     the checks, call PARSEDIAGNOSE to print any appropriate messages
C     and loop back to MEMOPARSE if PARSEDIAGNOSE's argument is true
C     (otherwise continue and process the data read in).  You don't need
C     to check for `END' or end-of-file.  Geddit?
C
C     Escape hatch: use PARSEKEYARG to get all the tokens after a random
C     keyword and call PARSE (or whatever) to deal with them as
C     necessary.  This is usually deprecated, however -- go for
C     simply-structured input.
C
C     10 call memoparse (.true.)  ! setup and echo i/p
C        call parseint ('IVAL', ival)  ! ival.eq.3 after `IVAL 3'
C        call parsecell (cell)   ! assign cell from `CELL 20 30 40' etc.
C        call parsekeyarg ('FRED', rest)  ! the full horror in REST
C        call parse (rest, ....)
C         [firkle with the `parse'd arrays]
C         ...
C        call parsediagnose (cont) ! check
C        if (cont) goto 10
C   c     now do something useful...
C
C     Fixme: the set of routines below might need extending...
C     Fixme: consider whether more obscure names worthwhile to avoid
C     possible clashes.
C
C     Dave Love $Date$
C     
      subroutine memoparse (echo)

C     Call PARSER and stash the returned values away for later testing
C     when the other entrypoints are called. (OK, so it's not really
C     memoisation...).
C
C     ECHO is set to echo the parser i/p.

      implicit none
C     Args
      character*(*) key, spgnam, pgname, rest
      logical echo, flag, cont
      integer ival, nsym, numsgp, nsymp, n, ivals (n)
      real rval, cell (6), rsym (4,4,*), resmin, resmax, smin, smax,
     +     rvals(n)

C     Stores for parser stuff
      integer maxtoks, maxline
C     Let's not mess around...
      parameter (maxtoks = 500, maxline=2000)
      integer ibeg(maxtoks), iend(maxtoks), ityp(maxtoks), idec(maxtoks)
      real fvalue (maxtoks)
      character*4 cvalue (maxtoks), memokey
      character line*(maxline)
      integer ntok

C     locals 
      logical someerr, eof, argerr, success
      integer i

      save
      data someerr, eof /2*.false./
      
      ntok=maxtoks
      argerr = .false.
      line = ' '
C     in case of immediate EOF:
      success = .true.
      call parser(memokey, line, ibeg, iend, ityp, fvalue, cvalue, idec,
     +     ntok, eof, echo)
C     END == EOF always
      if (memokey.eq.'END') eof = .true.
C     not sure if necessary:
      if (eof) memokey = ' '
      success = .false.
      return
      
      entry parsekey (key, flag)
C     bare KEY -- set FLAG if found
      if (memokey.eq.key) then
C       matched key
        if (ntok.eq.1) then
          success = .true.
          flag = .true.
        else
          argerr = .true.
          call lerror (1, 0, 'No argument expected')
        end if
      end if
      return

      entry parsekeyarg (key, rest)
C     KEY + rest of line -- returned in REST
      if (memokey.eq.key) then
C       matched key
        if (ntok.gt.1) then
          success = .true.
          rest = line (ibeg(2):iend(ntok))
        else
          argerr = .true.
          call lerror (1, 0, 'Argument expected')
        end if
      end if
      return

      entry parseint (key, ival)
C     KEY + integer -- returned in IVAL
      if (memokey.eq.key) then
C       matched key
        if (ntok.eq.2 .and. ityp (2).eq.2
     +       .and. idec (2).eq. (iend(2)-ibeg(2)+1)) then
          ival = nint (fvalue(2))
          success = .true.
        else 
          argerr = .true.
          call lerror (1, 0, 'Integer argument expected')
        end if
      end if
      return
      
      entry parsereal (key, rval)
C     KEY + real -- returned in RVAL
      if (memokey.eq.key) then
C       matched key
        if (ntok.eq.2 .and. ityp (2).eq.2) then
          rval = fvalue(2)
          success = .true.
        else 
          argerr = .true.
          call lerror (1, 0, 'Real argument expected')
        end if
      end if
      return
      
      entry parsenints (key, n, ivals)
C     KEY + upto N integers -- N reset to number found, returned in IVALS
      if (memokey.eq.key) then
        if (ntok.ge.2 .and. ntok.le. n+1) then
          do i = 1, min (n, ntok-1)
            if (ityp (i+1).ne.2) return
            ivals (i) = nint (fvalue (i+1))
            n = i
            success = .true.
          end do
        else
          argerr = .true.
          call lerror (1, 0, 'Incorrect number of integer arguments')
        end if
      end if
      return
      
      entry parsenreals (key, n, rvals)
C     KEY + upto N reals -- N reset to number found, returned in RVALS
      if (memokey.eq.key) then
        if (ntok.ge.2 .and. ntok.le. n+1) then
          do i = 1, min (n, ntok-1)
            if (ityp (i+1).ne.2) return
            rvals (i) = fvalue (i+1)
            n = i
            success = .true.
          end do
        else
          argerr = .true.
          call lerror (1, 0, 'Incorrect number of real arguments')
        end if
      end if
      return
      
      entry parsecell (cell)
C     CELL -- returned in CELL
      if (memokey.eq.'CELL') then
        call rdcell (2, ityp, fvalue, ntok, cell)
        success = .true.
      end if
      return
      
      entry parsesymm (spgnam, numsgp, pgname, nsym, nsymp, rsym)
C     SYMMetry -- usual values returned
      if (memokey.eq.'SYMM') then
        nsym = 0
        call rdsymm(2, line, ibeg, iend, ityp, fvalue, ntok, spgnam,
     +       numsgp, pgname, nsym, nsymp, rsym)
        success = .true.
      end if
      return
      
      entry parsereso (resmin, resmax, smin, smax)
C     RESOlution -- usual values returned
      if (memokey.eq.'RESO') then
        call rdreso (2, ityp, fvalue, ntok, resmin, resmax, smin, smax)
        success = .true.
      end if
      return
      
      entry parsediagnose (cont)
C     Call at end of tests for possible 'Invalid keyword' diagnostic or
C     abort if at EOF and had an error.  Continue processing (no EOF)
C     if (cont).
      if (.not.success .and. .not.argerr .and. .not.eof) then
        call lerror (1, 0, 'Invalid keyword')
        someerr = .true.
      else if (argerr) then
        someerr = .true.
      end if
      argerr = .false.
      if (eof) then
        cont = .false.
        if(someerr) call ccperr (1, 'Input error (see above)')
      else
        cont = .true.
      end if
      success = .false.
      return

      end

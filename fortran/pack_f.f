C
C     pack_f.f: (de)compress diffraction image files
C     Copyright (C) 1995  Jan P Abrahams
C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part 1' (Annex 2) software.
C     A copy of the CCP4 licence can be obtained by writing to the
C     CCP4 Secretary, Daresbury Laboratory, Warrington WA4 4AD, UK.
c
c
c
      subroutine pack_wordimage (data, x, y, filn)
c     =================================
c
c   Pack data stored in the array DATA with dimensions x * y in file FILN.
c
      implicit none
c
      character*(*) filn
      integer*4 x, y, j
      integer*2 data(x, y)
      integer*4 filnarray(1025)
c
c     ..
c     .. External C-routine
      external pack_wordimage_f

      do 10, j = 1, len(filn)
         if (filn(j:j) .ne. ' ') then
            filnarray(j) = ichar(filn(j:j))
         else
            filnarray(j) = 0
         endif
 10   continue
      filnarray(len(filn) + 1) = 0
      call pack_wordimage_f(data, x, y, filnarray)
      return
      end

c*******************************************************************************
c
c
c
      subroutine v2pack_wordimage (data, x, y, filn)
c     =================================
c
c   Pack data stored in the array DATA with dimensions x * y in file FILN.
c
      implicit none
c
      character*(*) filn
      integer*4 x, y, j
      integer*2 data(x, y)
      integer*4 filnarray(1025)
c
c     ..
c     .. External C-routine
      external v2pack_wordimage_f

      do 10, j = 1, len(filn)
         if (filn(j:j) .ne. ' ') then
            filnarray(j) = ichar(filn(j:j))
         else
            filnarray(j) = 0
         endif
 10   continue
      filnarray(len(filn) + 1) = 0
      call v2pack_wordimage_f(data, x, y, filnarray)
      return
      end

c*******************************************************************************
c
c
c
      subroutine pack_longimage (data, x, y, filn)
c     =================================
c
c   Pack data stored in the array DATA with dimensions x * y in file FILN.
c
      implicit none
c
      character*(*) filn
      integer*4 x, y, j
      integer*4 data(x, y)
      integer*4 filnarray(1025)
c
c     ..
c     .. External C-routine
      external pack_wordimage_f
c
      do 10, j = 1, len(filn)
         if (filn(j:j) .ne. ' ') then
            filnarray(j) = ichar(filn(j:j))
         else
            filnarray(j) = 0
         endif
 10   continue
      filnarray(len(filn) + 1) = 0
      call pack_wordimage_f(data, x, y, filnarray)
      return
      end

c*******************************************************************************
c
c
c
      subroutine v2pack_longimage (data, x, y, filn)
c     =================================
c
c   Pack data stored in the array DATA with dimensions x * y in file FILN.
c
      implicit none
c
      character*(*) filn
      integer*4 x, y, j
      integer*4 data(x, y)
      integer*4 filnarray(1025)
c
c     ..
c     .. External C-routine
      external v2pack_wordimage_f
c
      do 10, j = 1, len(filn)
         if (filn(j:j) .ne. ' ') then
            filnarray(j) = ichar(filn(j:j))
         else
            filnarray(j) = 0
         endif
 10   continue
      filnarray(len(filn) + 1) = 0
      call v2pack_wordimage_f(data, x, y, filnarray)
      return
      end

c*******************************************************************************
c
c
c
      subroutine readpack_word (data, filn)
c     =================================
c
c     Read a packed image from file 'filn' into array 'data'. If you want 
c     to generate the mirror-image, (interchange first and last stripes, etc.)
c     call "mirror_wordimg(data, nfast, nslow)", where nfast and nslow contain
c     the number of fast and slow indices, after reading the packed image.
c
      implicit none
c
      character*(*) filn
      integer*2 data, j
      integer*4 filnarray(1025)
c
c     ..
c     .. External C-routine
      external readpack_word_f
c
      do 10, j = 1, len(filn)
         if (filn(j:j) .ne. ' ') then
            filnarray(j) = ichar(filn(j:j))
         else
            filnarray(j) = 0
         endif
 10   continue
      filnarray(len(filn) + 1) = 0
      call readpack_word_f(data, filnarray)
      return
      end

c*******************************************************************************
c
c
c
      subroutine readpack_long (data, filn)
c     =================================
c
c     Read a packed image from file 'filn' into array 'data'. If you want 
c     to generate the mirror-image, (interchange first and last stripes, etc.)
c     call "mirror_wordimg(data, nfast, nslow)", where nfast and nslow contain
c     the number of fast and slow indices, after reading the packed image.
c
      implicit none
c
      character*(*) filn
      integer*4 data, j
      integer*4 filnarray(1025)
c
c     ..
c     .. External C-routine
      external readpack_long_f
c
      do 10, j = 1, len(filn)
         if (filn(j:j) .ne. ' ') then
            filnarray(j) = ichar(filn(j:j))
         else
            filnarray(j) = 0
         endif
 10   continue
      filnarray(len(filn) + 1) = 0
      call readpack_long_f(data, filnarray)
      return
      end

c*******************************************************************************
c
c
c
      subroutine imsiz (filn, x, y)
c     =================================
c
c     Determines the size of the the packed image "filename" after 
c     unpacking.The dimensions are returned in x and y. 
c     Read a packed image from file 'filn' into array 'data'.
c
      implicit none
c
      character*(*) filn
      integer*4 x, y, j
      integer*4 filnarray(1025)
c
c     ..
c     .. External C-routine
      external imsiz_f
c
      do 10, j = 1, len(filn)
         if (filn(j:j) .ne. ' ') then
            filnarray(j) = ichar(filn(j:j))
         else
            filnarray(j) = 0
         endif
 10   continue
      filnarray(len(filn) + 1) = 0
      call imsiz_f(filnarray, x, y)
      return
      end

c*******************************************************************************








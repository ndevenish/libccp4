C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C     ***** NOTE ON DOCUMENTATION *****
C     Lines beginning "CDOC" will be extracted to form cciflib.html
C
      subroutine ccp4ccif_init

CDOC  This subroutine initialises things and loads the CIF dictionary.
CDOC  This subroutine must be called before any other in cciflib.f !!

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER FMTSTAT

      DATA IATOM_CONTEXT/MAXBLK*-1/,IATOM_STATUS/MAXBLK*0/
      
c_______The symbol table contains dictionary information on each data item,
c       which is accessed with a hashed lookup on the data name.
      call ccif_init('MMCIFDIC')

C---Category formatting.
C   'n' forces newline.
      CALL CCIF_OUTPUT_FMT('ATOM_SITE',' ',0,0,'n',fmtstat)

C---Data item formatting. Maybe this should be a user-accessible
C   resource file?
C   Widths compensate for lengths of names.
C   2nd parameter: '-' means left justify, ' ' means prepend space
      CALL CCIF_OUTPUT_FMT('_audit.revision_id',' ',29,0,'s',fmtstat)
      CALL CCIF_OUTPUT_FMT('_audit.creation_date',' ',27,0,'s',fmtstat)
      CALL CCIF_OUTPUT_FMT('_audit.creation_method',' ',25,0,'s',
     +                                                       fmtstat)
      CALL CCIF_OUTPUT_FMT('_symmetry.Int_Tables_number',' ',
     +                                           18,0,'i',fmtstat)
      CALL CCIF_OUTPUT_FMT('_symmetry.space_group_name_H-M',' ',
     +                                           15,0,'s',fmtstat)
      CALL CCIF_OUTPUT_FMT('_cell.length_a',' ',18,3,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_cell.length_b',' ',18,3,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_cell.length_c',' ',18,3,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_cell.angle_alpha',' ',15,3,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_cell.angle_beta',' ',16,3,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_cell.angle_gamma',' ',15,3,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_cell.volume',' ',20,3,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_sites.Fract_transf_matrix[1][1]',
     +                                          '-',10,6,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_sites.Fract_transf_matrix[1][2]',
     +                                          '-',10,6,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_sites.Fract_transf_matrix[1][3]',
     +                                          '-',10,6,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_sites.Fract_transf_vector[1]',
     +                                          '-',50,6,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_sites.Fract_transf_matrix[2][1]',
     +                                          '-',10,6,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_sites.Fract_transf_matrix[2][2]',
     +                                          '-',10,6,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_sites.Fract_transf_matrix[2][3]',
     +                                          '-',10,6,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_sites.Fract_transf_vector[2]',
     +                                          '-',50,6,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_sites.Fract_transf_matrix[3][1]',
     +                                          '-',10,6,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_sites.Fract_transf_matrix[3][2]',
     +                                          '-',10,6,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_sites.Fract_transf_matrix[3][3]',
     +                                          '-',10,6,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_sites.Fract_transf_vector[3]',
     +                                          '-',50,6,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_site.group_PDB','-',4,0,'s',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_site.id',' ',7,0,'s',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_site.label_atom_id',
     +                                            ' ',5,0,'s',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_site.label_alt_id',
     +                                            ' ',1,0,'s',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_site.label_comp_id',
     +                                            ' ',3,0,'s',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_site.label_asym_id',
     +                                            ' ',2,0,'s',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_site.auth_seq_id',' ',5,0,'s',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_site.type_symbol',' ',4,0,'s',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_site.label_seq_id',
     +                                            ' ',5,0,'i',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_site.Cartn_x',' ',8,3,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_site.Cartn_y',' ',8,3,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_site.Cartn_z',' ',8,3,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_site.occupancy',' ',6,2,'f',fmtstat)
      CALL CCIF_OUTPUT_FMT('_atom_site.B_iso_or_equiv',
     +                                        ' ',6,2,'f',fmtstat)
      
      return
      end

      subroutine ccp4ccif_ROPEN(LOGNAM,REQUESTED_BLOCK,blk_id)

CDOC  Open a CIF file for reading.
CDOC
CDOC  Arguments :
CDOC
CDOC  LOGNAM          (I)     CHARACTER       logical name of file to be 
CDOC                                          opened
CDOC
CDOC  REQUESTED_BLOCK (I)     CHARACTER       name of data block to be
CDOC                                          opened. If blank, then first
CDOC                                          one in file is opened.
CDOC
CDOC  BLK_ID          (O)     INTEGER         block ID of data for future
CDOC                                          reference

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER NBLOCK,blk_id,istat,LENSTR,IDX,ifail
      CHARACTER*(*) LOGNAM,REQUESTED_BLOCK
      character*(cfllen) blknam
      character errline*80,disposition*3

      WRITE(6,'('' Loading CIF file....'')')
      ifail = 0
      call ccif_load_cif(LOGNAM, nblock,ifail)
      WRITE(6,'('' Done ['',i3,'' block(s)]'')') nblock

c_______Was a particular block requested?
      IF (REQUESTED_BLOCK.NE.' ') THEN
        blknam = 'data_'//REQUESTED_BLOCK(1:LENSTR(REQUESTED_BLOCK))
        disposition = ' '
        call ccif_block_by_name(LOGNAM,blknam,blk_id,istat,disposition)
        IF ( istat.le.0 ) THEN
          WRITE(ERRLINE,*) 'Cannot find block ',
     + blknam(1:LENSTR(blknam)),' in ',LOGNAM(1:LENSTR(LOGNAM))
         call ccperr(1,ERRLINE)
        ENDIF
      ELSE
        IDX = 1
        call ccif_block_by_index(LOGNAM,IDX,blknam,blk_id,istat)
      ENDIF
      
C Now we check for the presence of certain categories, and
C set up sort if present
      LANISOTROP(blk_id) = .FALSE.
      ISTAT = 0
      call ccp4ccif_setup_context('ATOM_SITE_ANISOTROP',blk_id,
     +        IANISOTROP_CONTEXT(blk_id),istat,' ')
      IF (ISTAT.GT.0) THEN
        CALL CCIF_SETUP_SORT('ATOM_SITE_ANISOTROP',blk_id,
     +                         IANISOTROP_SORT(blk_id),ISTAT)
        IF (ISTAT.EQ.0) GOTO 100
        CALL CCIF_ADD_TO_SORT('_atom_site_anisotrop.id',
     +                         IANISOTROP_SORT(blk_id),ISTAT)
        IF (ISTAT.EQ.0) GOTO 100
        CALL CCIF_DO_SORT(IANISOTROP_SORT(blk_id),ISTAT)
        IF (ISTAT.EQ.0) GOTO 100
        LANISOTROP(blk_id) = .TRUE.
      ENDIF

 100  CONTINUE

      return
      end

      subroutine ccp4ccif_RCLOSE(LOGNAM)

CDOC  Close a CIF file opened for reading.
CDOC
CDOC  Arguments :
CDOC
CDOC  LOGNAM          (I)     CHARACTER       logical name of file to be 
CDOC                                          closed

      IMPLICIT NONE

      include 'cciflib.fh'

      CHARACTER*(*) LOGNAM

      INTEGER istat
      CHARACTER LINE*200

      ISTAT = 0
      call ccif_close_cif(LOGNAM,istat)
      WRITE(LINE,'(A,A,A)') 'File ',LOGNAM,' closed.'
      CALL CCPERR(4,LINE)

      return
      end

      subroutine ccp4ccif_WOPEN(LOGNAM,LOGNAMIN,REQUESTED_BLOCK,blk_id)

CDOC  Open a CIF file for writing.
CDOC
CDOC  Arguments :
CDOC
CDOC  LOGNAM          (I)     CHARACTER       logical name of file to be 
CDOC                                          opened for writing.
CDOC
CDOC  LOGNAMIN        (I)     CHARACTER       logical name of corresponding
CDOC                                          file opened for reading. If this
CDOC                                          is given, output file is initialised
CDOC                                          with contents of input file.
CDOC
CDOC  REQUESTED_BLOCK (I)     CHARACTER       name of data block to be
CDOC                                          written. 
CDOC
CDOC  BLK_ID          (O)     INTEGER         block ID of data for future
CDOC                                          reference

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER NBLOCK,blk_id,istat,LENSTR,IDX
      CHARACTER*(*) LOGNAM,LOGNAMIN,REQUESTED_BLOCK
      character*(cfllen) blknam
      character errline*80,disposition*3

      WRITE(6,'('' Writing CIF file....'')')
      call ccif_new_cif(LOGNAMIN,LOGNAM,nblock)
      WRITE(6,'('' Done ['',i3,'' block(s)]'')') nblock

c_______Was a particular block requested?
      IF (REQUESTED_BLOCK.NE.' ') THEN
        blknam = 'data_'//REQUESTED_BLOCK(1:LENSTR(REQUESTED_BLOCK))
        disposition = 'NEW'
        call ccif_block_by_name(LOGNAM,blknam,blk_id,istat,disposition)
        IF ( istat.le.0 ) THEN
          WRITE(ERRLINE,*) 'Cannot find block ',
     + blknam(1:LENSTR(blknam)),' in ',LOGNAM(1:LENSTR(LOGNAM))
         call ccperr(1,ERRLINE)
        ENDIF
      ELSE
        IDX = 1
        call ccif_block_by_index(LOGNAM,IDX,blknam,blk_id,istat)
      ENDIF
      
C If output based on an input file, we check for the presence of 
C certain categories, and set up sort if present
      LANISOTROP(blk_id) = .FALSE.
      IF (LOGNAMIN.EQ.' ') GOTO 100    

C This context can't be used to change values, hence 'RO'
      ISTAT = 0
      call ccp4ccif_setup_context('ATOM_SITE_ANISOTROP',blk_id,
     +        IANISOTROP_CONTEXT(blk_id),istat,'RO')
      IF (ISTAT.GT.0) THEN
        CALL CCIF_SETUP_SORT('ATOM_SITE_ANISOTROP',blk_id,
     +                         IANISOTROP_SORT(blk_id),ISTAT)
        IF (ISTAT.EQ.0) GOTO 100
        CALL CCIF_ADD_TO_SORT('_atom_site_anisotrop.id',
     +                         IANISOTROP_SORT(blk_id),ISTAT)
        IF (ISTAT.EQ.0) GOTO 100
        CALL CCIF_DO_SORT(IANISOTROP_SORT(blk_id),ISTAT)
        IF (ISTAT.EQ.0) GOTO 100
        LANISOTROP(blk_id) = .TRUE.
      ENDIF

 100  CONTINUE
     
      return
      end

      subroutine ccp4ccif_WCLOSE(LOGNAM)

CDOC  Close a CIF file opened for writing.
CDOC
CDOC  Arguments :
CDOC
CDOC  LOGNAM          (I)     CHARACTER       logical name of file to be 
CDOC                                          closed

      IMPLICIT NONE

      include 'cciflib.fh'

      CHARACTER*(*) LOGNAM

      INTEGER istat
      CHARACTER LINE*200

      ISTAT = 0
      call ccif_print_cif(LOGNAM)
      call ccif_close_cif(LOGNAM,istat)
      WRITE(LINE,'(A,A,A)') 'File ',LOGNAM,' closed.'
      CALL CCPERR(4,LINE)
     
      return
      end

      subroutine ccp4ccif_getsymmetry(blk_id,
     +                   NumSpaceGroup,SpaceGroupName,IFAIL)

CDOC  Get symmetry information for given data block.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  NumSpaceGroup   (O)     INTEGER         spacegroup number
CDOC
CDOC  SpaceGroupName  (O)     CHARACTER       spacegroup name
CDOC
CDOC  IFAIL           (O)     INTEGER         =0 OK
      
C     Need infer one from other if not both in CIF file
C     Use MSYMLB ??? But that has P212121 rather than P 21 21 21
C     Local lookup table or extend symop.lib

      IMPLICIT NONE

      include 'cciflib.fh'
      
      integer i, blk_id,length_dep,angle_dep,ncntxt,istat,lenval,
     + btype,sline,istate,IFAIL,istatus,ival,NumSpaceGroup,
     + NSYMP,NSYM
      REAL ROT(4,4,192)
      character*8 PointGroupName
      character*(cfllen) val, esdval, blknam, cval,
     & symm_itmnam(2), symm_catnam, itmnam
      character*(*) SpaceGroupName
      
      IFAIL = 0

      symm_itmnam(1) = '_symmetry.Int_Tables_number'
      symm_itmnam(2) = '_symmetry.space_group_name_H-M'
 
C--- Set-up context for SYMM category
      symm_catnam = 'SYMMETRY'

      NumSpaceGroup = 0
      SpaceGroupName = ' '

      istatus = 0
      call ccp4ccif_setup_context(symm_catnam,blk_id,ncntxt,istatus,' ')
      IF (ISTATUS.EQ.cat_not_present) THEN
        WRITE(6,*) ' Warning: no symmetry information in file!'
        IFAIL = 1
        RETURN
      ENDIF

C--- symmetry number

      itmnam = symm_itmnam(1)

c_____Check type of data item (real, integer, etc.)
      btype = 2
      call ccp4ccif_check_type(itmnam, btype, sline)

      istat = keep_context
      call ccif_get_int(itmnam, val, ival, ncntxt, istat)
      if (istat.eq.single_value) then
        NumSpaceGroup = ival
      endif

C--- symmetry name

      itmnam = symm_itmnam(2)

c_____Check type of data item (real, integer, etc.)
      btype = 1
      call ccp4ccif_check_type(itmnam, btype, sline)

      istat = keep_context
      call ccif_get_char(itmnam, val, cval, lenval,
     &      ncntxt, istat)
      if (istat.eq.single_value) then
        SpaceGroupName = cval
      endif

      IF (NumSpaceGroup.EQ.0 .AND. SpaceGroupName.NE.' ') THEN
        CALL MSYMLB2(24,NumSpaceGroup,SpaceGroupName,
     +        PointGroupName,NSYMP,NSYM,ROT)
        WRITE(6,*) ' Spacegroup number inferred from name.'
      ELSEIF (NumSpaceGroup.NE.0 .AND. SpaceGroupName.EQ.' ') THEN
        CALL MSYMLB2(24,NumSpaceGroup,SpaceGroupName,
     +        PointGroupName,NSYMP,NSYM,ROT)
        WRITE(6,*) ' Spacegroup name inferred from number.'
      ELSEIF (NumSpaceGroup.EQ.0 .AND. SpaceGroupName.EQ.' ') THEN
        WRITE(6,*) ' Warning: no symmetry information in file!'
        IFAIL = 1
      ENDIF

      CALL CCIF_RELEASE_CONTEXT(NCNTXT)

      return
      end

      subroutine ccp4ccif_getentity(blk_id,entityid,entitytype,IFAIL)

CDOC  Get entity information for given data block.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  entityid        (O)     CHARACTER       entity identifier
CDOC
CDOC  entitytype      (O)     CHARACTER       entity type: 'polymer',
CDOC                                          'non-polymer' or 'water'
CDOC
CDOC  IFAIL           (O)     INTEGER         =0 OK
      
c     Given a block_id, return _entity.id and _entity.type

      IMPLICIT NONE

      include 'cciflib.fh'
      
      integer i, blk_id,ncntxt,istat,lenval,
     & btype,sline,IFAIL,istatus,ival
      character*(cfllen) val,cval,entity_itmnam(2),entity_catnam
      character*(*) entityid(*),entitytype(*)
      
      entity_itmnam(1) = '_entity.id'
      entity_itmnam(2) = '_entity.type'

c_____Check type of data item is character
      btype = 1
      call ccp4ccif_check_type(entity_itmnam(1), btype, sline)
c_____Check type of data item is character
      btype = 1
      call ccp4ccif_check_type(entity_itmnam(2), btype, sline)
 
C--- Set-up context for ENTITY category
      entity_catnam = 'ENTITY'

      istatus = 0
      call ccp4ccif_setup_context(entity_catnam,blk_id,ncntxt,
     +                                                 istatus,' ')
      IF (ISTATUS.EQ.cat_not_present) THEN
        WRITE(6,*) ' Warning: no entity information in file!'
        IFAIL = 1
        RETURN
      ENDIF

      i = 0
      istat = keep_context
 10   i = i + 1

C--- entity id
      call ccif_get_char(entity_itmnam(1), val, cval, lenval,
     &      ncntxt, istat)
      if (istat.eq.end_of_context) then
        goto 20
      elseif (istat.eq.loop_value) then
        entityid(i) = cval
      else
        entityid(i) = val
      endif

      istat = keep_context
C--- entity type
      call ccif_get_char(entity_itmnam(2), val, cval, lenval,
     &      ncntxt, istat)
      if (istat.eq.end_of_context) then
        goto 20
      elseif (istat.eq.loop_value) then
        entitytype(i) = cval
      else
        entitytype(i) = val
      endif

      istat = advance_context
      goto 10

 20   continue

      CALL CCIF_RELEASE_CONTEXT(NCNTXT)

      return
      end

      subroutine ccp4ccif_getstructasym(blk_id,asymid,asymentityid,
     +                                        entitytype,IFAIL)

CDOC  Get information about the contents of the a.s.u. for 
CDOC  given data block.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  asymid          (O)     CHARACTER       _struct_asym.id
CDOC                                          parent of _atom_site.label_asym_id 
CDOC
CDOC  asymentityid    (O)     CHARACTER       _struct_asym.entity_id
CDOC                                          child of _entity.id
CDOC
CDOC  entitytype      (O)     CHARACTER       _entity.type
CDOC                                          entity type: 'polymer',
CDOC                                          'non-polymer' or 'water'
CDOC
CDOC  IFAIL           (O)     INTEGER         =0 OK

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER MAXENTITY
      PARAMETER (MAXENTITY=100)
      
      integer i,J,blk_id,ncntxt,istat,lenval,NASYM,
     & btype,sline,IFAIL,istatus,ival
      character*(cfllen) val,cval,asym_itmnam(2),asym_catnam
      character*(*) asymid(*),asymentityid(*),entitytype(*)
      character*(cfllen) entid(MAXENTITY),enttype(MAXENTITY),id
      
      asym_itmnam(1) = '_struct_asym.id'
      asym_itmnam(2) = '_struct_asym.entity_id'

c_____Check type of data item is character
      btype = 1
      call ccp4ccif_check_type(asym_itmnam(1), btype, sline)
c_____Check type of data item is character
      btype = 1
      call ccp4ccif_check_type(asym_itmnam(2), btype, sline)
 
C--- Set-up context for ASYM category
      asym_catnam = 'STRUCT_ASYM'

      istatus = 0
      call ccp4ccif_setup_context(asym_catnam,blk_id,ncntxt,istatus,' ')
      IF (ISTATUS.EQ.cat_not_present) THEN
        WRITE(6,*) ' Warning: no struct_asym information in file!'
        IFAIL = 1
        RETURN
      ENDIF

      i = 0
      istat = keep_context
 10   i = i + 1

C--- _struct_asym.id
      call ccif_get_char(asym_itmnam(1), val, cval, lenval,
     &      ncntxt, istat)
      if (istat.eq.end_of_context) then
        goto 20
      elseif (istat.eq.loop_value) then
        asymid(i) = cval
      else
        asymid(i) = val
      endif

      istat = keep_context
C--- _struct_asym.entity_id
      call ccif_get_char(asym_itmnam(2), val, cval, lenval,
     &      ncntxt, istat)
      if (istat.eq.end_of_context) then
        goto 20
      elseif (istat.eq.loop_value) then
        asymentityid(i) = cval
      else
        asymentityid(i) = val
      endif

      istat = advance_context
      goto 10

 20   continue
      NASYM = I - 1

      CALL CCIF_RELEASE_CONTEXT(NCNTXT)

C--- now get corresponding entity entries
      DO I = 1,MAXENTITY
        entid(I) = ' '
        enttype(I) = ' '
      ENDDO
      CALL ccp4ccif_getentity(blk_id,entid,enttype,IFAIL)
      IF (IFAIL.NE.0) RETURN

      DO 30 I = 1,NASYM
        ID = asymentityid(i)
        DO 40 J = 1,MAXENTITY
          IF (ENTID(J).EQ.ID) THEN
            entitytype(I) = enttype(J)
            GOTO 30
          ENDIF
 40     CONTINUE
 30   CONTINUE

      return
      end

      subroutine ccp4ccif_getcell(blk_id,cell,vol,IFAIL)

CDOC  Get cell information for given data block.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  cell(6)         (O)     REAL            cell dimensions
CDOC
CDOC  vol             (O)     REAL            cell volume
CDOC
CDOC  IFAIL           (O)     INTEGER         =0 OK

      IMPLICIT NONE

      include 'cciflib.fh'
      
      integer i, blk_id,length_dep,angle_dep,ncntxt,istat,
     & btype,sline,istate,IFAIL,istatus
      character*(cfllen) val, esdval, blknam, 
     & cell_itmnam(7), cell_catnam, itmnam
      real cell(6),vol,vol_from_file,vol_check,rval,esd,
     + CONV,ALPH,BET,GAMM,SUM

      vol_check = 0.01

      length_dep = 0
      angle_dep = 0

      cell_itmnam(1) = '_cell.length_a'
      cell_itmnam(2) = '_cell.length_b'
      cell_itmnam(3) = '_cell.length_c'
      cell_itmnam(4) = '_cell.angle_alpha'
      cell_itmnam(5) = '_cell.angle_beta'
      cell_itmnam(6) = '_cell.angle_gamma'
      cell_itmnam(7) = '_cell.volume'
 
C--- Set-up context for CELL category
      cell_catnam = 'CELL'

      istatus = item_context
      call ccp4ccif_setup_context(cell_catnam,blk_id,ncntxt,istatus,' ')
      IF (ISTATUS.EQ.cat_not_present) THEN
        WRITE(6,*) ' Warning: no cell information in file!'
        IFAIL = 1
        RETURN
      ENDIF

C--- Cell lengths

      DO I=1,3

        itmnam = cell_itmnam(I)

c_______Check type of data item is real
        btype = 4
        call ccp4ccif_check_type(itmnam, btype, sline)

        istat = keep_context
        call ccif_get_real_esd(itmnam, val, rval, esdval, esd, 
     &      ncntxt, istat, istate)
        if (istat.eq.single_value) then
          length_dep = length_dep + 1
          cell(I) = rval
        endif

      ENDDO

      IF (length_dep.EQ.0) THEN
        WRITE(6,*) ' Warning: no cell lengths given in file!'
        RETURN
      ELSEIF (length_dep.LT.3) THEN
        WRITE(6,*) ' Warning: not all cell lengths given in file!'
        cell(1) = 0.0
        cell(2) = 0.0
        cell(3) = 0.0
        RETURN
      ENDIF  

C--- Cell angles

      DO I=4,6

        itmnam = cell_itmnam(I)

c_______Check type of data item is real
        btype = 4
        call ccp4ccif_check_type(itmnam, btype, sline)

        istat = keep_context
        call ccif_get_real_esd(itmnam, val, rval, esdval, esd, 
     &      ncntxt, istat, istate)
        if (istat.eq.single_value) then
          angle_dep = angle_dep + 1
          cell(I) = rval
        endif

      ENDDO

      IF (angle_dep.EQ.0) THEN
        WRITE(6,*) ' Warning: no cell angles given in file!'
        WRITE(6,*) ' Warning: cell angles default to 90'
        cell(4) = 90.0
        cell(5) = 90.0
        cell(6) = 90.0
        RETURN
      ELSEIF (angle_dep.LT.3) THEN
        WRITE(6,*) ' Warning: not all cell angles given in file!'
        WRITE(6,*) ' Warning: cell angles default to 90'
        cell(4) = 90.0
        cell(5) = 90.0
        cell(6) = 90.0
        RETURN
      ENDIF  

C--- Derive cell volume
      CONV = ATAN(1.0)*4.0/180.0
      ALPH = CELL(4)*CONV
      BET = CELL(5)*CONV
      GAMM = CELL(6)*CONV
      SUM = (ALPH+BET+GAMM)*0.5
      vol = 2.0*CELL(1)*CELL(2)*CELL(3)*
     +    SQRT(SIN(SUM-ALPH)*SIN(SUM-BET)*SIN(SUM-GAMM)*SIN(SUM))

C--- Now check this against cell volume in file if there.
C    This is just a check: I see no reason to return vol_from_file
C    instead of vol.

      itmnam = cell_itmnam(7)
c_______Check type of data item is real
      btype = 4
      call ccp4ccif_check_type(itmnam, btype, sline)

      istat = keep_context
      call ccif_get_real_esd(itmnam, val, rval, esdval, esd, 
     &      ncntxt, istat, istate)
      if (istat.eq.single_value) then
C--- Volume is in file
        vol_from_file = rval
        if (abs(vol_from_file-vol)/vol.gt.vol_check) then
          write(6,*) ' Warning: disagreement between cell volume',
     +      ' and cell dimensions in file.'
        endif
      endif

      CALL CCIF_RELEASE_CONTEXT(NCNTXT)

      return
      end

      subroutine ccp4ccif_getrcell(blk_id,rcell,rvol,IFAIL)

CDOC  Get reciprocal cell information for given data block.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  rcell(6)        (O)     REAL            reciprocal cell dimensions
CDOC
CDOC  rvol            (O)     REAL            reciprocal cell volume
CDOC
CDOC  IFAIL           (O)     INTEGER         =0 OK

      IMPLICIT NONE

      integer ifail,blk_id
      real cell(6),vol,rcell(6),rvol

C Get cell info for given data block
      call ccp4ccif_getcell(blk_id,cell,vol,IFAIL)

C Calculate reciprocal cell
      call ccp4ccif_cellinvert(cell,vol,rcell,rvol)

      return
      end

      subroutine ccp4ccif_cellinvert(cell,vol,rcell,rvol)

CDOC  Get reciprocal cell from real cell
CDOC
CDOC  Arguments :
CDOC
CDOC  cell(6)         (I)     REAL            cell dimensions
CDOC
CDOC  vol             (I)     REAL            cell volume
CDOC
CDOC  rcell(6)        (O)     REAL            reciprocal cell dimensions
CDOC
CDOC  rvol            (O)     REAL            reciprocal cell volume
CDOC

      IMPLICIT NONE

      real cell(6),vol,rcell(6),rvol,CONV,ALPH,BET,GAMM,
     +  SINA,COSA,SINB,COSB,SING,COSG,A,B,C,SUM,
     +  SINAS,COSAS,SINBS,COSBS,SINGS,COSGS

      CONV = ATAN(1.0)*4.0/180.0
      ALPH = CELL(4)*CONV
      BET = CELL(5)*CONV
      GAMM = CELL(6)*CONV

C Calculate volume if not input
      IF (VOL.LE.0.0) THEN
        SUM = (ALPH+BET+GAMM)*0.5
        VOL = 2.0*CELL(1)*CELL(2)*CELL(3)*
     +    SQRT(SIN(SUM-ALPH)*SIN(SUM-BET)*SIN(SUM-GAMM)*SIN(SUM))
      ENDIF

      SINA = SIN(ALPH)
      COSA = COS(ALPH)
      SINB = SIN(BET)
      COSB = COS(BET)
      SING = SIN(GAMM)
      COSG = COS(GAMM)
      COSAS = (COSG*COSB-COSA)/ (SINB*SING)
      SINAS = SQRT(1.0-COSAS*COSAS)
      COSBS = (COSA*COSG-COSB)/ (SINA*SING)
      SINBS = SQRT(1.0-COSBS*COSBS)
      COSGS = (COSA*COSB-COSG)/ (SINA*SINB)
      SINGS = SQRT(1.0-COSGS*COSGS)
      A = CELL(1)
      B = CELL(2)
      C = CELL(3)
      RCELL(1) = B*C*SINA/VOL
      RCELL(2) = C*A*SINB/VOL
      RCELL(3) = A*B*SING/VOL
      RCELL(4) = ATAN2(SINAS,COSAS)/CONV
      RCELL(5) = ATAN2(SINBS,COSBS)/CONV
      RCELL(6) = ATAN2(SINGS,COSGS)/CONV

      RVOL = 1.0/VOL

      return
      end

      subroutine ccp4ccif_getmatrices(blk_id,RO,RF)

CDOC  Get orthogonalising and fractionalising matrices for given data block.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  RO(4,4)         (O)     REAL            orthogonalising matrix
CDOC
CDOC  RF(4,4)         (O)     REAL            fractionalising matrix
CDOC
CDOC  If file has only one, this is used to derive other.
CDOC  If file has neither, both are derived from cell assuming NCODE = 1.

C     Could maybe derive orthogonalisation code 
C     from _atom_sites.cartn_transform_axes but that is probably
C     unreliable

C if matrices present, read and check against cell - derive NCODE

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER blk_id,IFAIL1,IFAIL2,NCODE,ncntxt,istat,
     & btype,sline,istate,IFAIL,istatus
      REAL RF(4,4),RO(4,4),CELL(6),vol
      character*(cfllen) val, esdval, blknam, 
     & mat_itmnam(7), mat_catnam, itmnam

C--- See if orthogonalising matrices are in file
      CALL ccp4ccif_getorthomat(blk_id,RO,IFAIL1)

C--- See if fractionalising matrices are in file
      CALL ccp4ccif_getfracmat(blk_id,RF,IFAIL2)

C--- If only one present, derive other
      IF (IFAIL1.EQ.0 .AND. IFAIL2.NE.0) THEN
        CALL ccp4ccif_MAT4INV(RO,RF)
        WRITE(6,'(/,A,/)') 'RO in file, deriving RF ...'
      ELSEIF (IFAIL1.NE.0 .AND. IFAIL2.EQ.0) THEN
        CALL ccp4ccif_MAT4INV(RF,RO)
        WRITE(6,'(/,A,/)') 'RF in file, deriving RO ...'
C--- If neither present, derive from cell assuming NCODE = 1
      ELSEIF (IFAIL1.NE.0 .AND. IFAIL2.NE.0) THEN
        CALL ccp4ccif_getcell(blk_id,cell,vol,IFAIL)
        NCODE = 1
        CALL ccp4ccif_CELL2MAT(CELL,NCODE,RO,RF)
        WRITE(6,'(/,A,/)') 
     +    'Deriving RO and RF from cell, assuming NCODE = 1'
      ENDIF

      return
      end

      subroutine ccp4ccif_getorthomat(blk_id,RO,IFAIL)

CDOC  Get orthogonalising matrix for given data block.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  RO(4,4)         (O)     REAL            orthogonalising matrix
CDOC
CDOC  IFAIL           (O)     INTEGER         =0 OK
CDOC                                          =1 not found in file

      IMPLICIT NONE

      include 'cciflib.fh'
      
      integer i,J,K,blk_id,ncntxt,istat,
     & btype,sline,IFAIL,istatus
      character*(cfllen) val, blknam, 
     & mat_itmnam(9), vec_itmnam(9), mat_catnam, itmnam
      real RO(4,4),rval

      mat_itmnam(1) = '_atom_sites.Cartn_transf_matrix[1][1]'
      mat_itmnam(2) = '_atom_sites.Cartn_transf_matrix[1][2]'
      mat_itmnam(3) = '_atom_sites.Cartn_transf_matrix[1][3]'
      mat_itmnam(4) = '_atom_sites.Cartn_transf_matrix[2][1]'
      mat_itmnam(5) = '_atom_sites.Cartn_transf_matrix[2][2]'
      mat_itmnam(6) = '_atom_sites.Cartn_transf_matrix[2][3]'
      mat_itmnam(7) = '_atom_sites.Cartn_transf_matrix[3][1]'
      mat_itmnam(8) = '_atom_sites.Cartn_transf_matrix[3][2]'
      mat_itmnam(9) = '_atom_sites.Cartn_transf_matrix[3][3]'
      vec_itmnam(1) = '_atom_sites.Cartn_transf_vector[1]'
      vec_itmnam(2) = '_atom_sites.Cartn_transf_vector[2]'
      vec_itmnam(3) = '_atom_sites.Cartn_transf_vector[3]'
 
C--- Set-up context for ATOM_SITES category
      mat_catnam = 'ATOM_SITES'

C--- ATOM_SITES can appear as items or loop
      istatus = 0
      call ccp4ccif_setup_context(mat_catnam,blk_id,ncntxt,istatus,' ')
      IF (ISTATUS.EQ.cat_not_present) THEN
        IFAIL = 1
        RETURN
      ENDIF

C--- Matrix elements

      K = 0
      DO I=1,3
       DO J=1,3
        K = K + 1

        itmnam = mat_itmnam(k)

c_______Check type of data item is real
        btype = 3
        call ccp4ccif_check_type(itmnam, btype, sline)

        istat = keep_context
        call ccif_get_real(itmnam, val, rval, ncntxt, istat)
        if (istat.eq.loop_value.or.istat.eq.single_value) then
          RO(I,J) = rval
        else
          IFAIL = 1
          RETURN
        endif

       ENDDO
      ENDDO

C--- Vector elements 
C    (if matrix was present, but not vector, assume it is zero)

      DO I=1,3

        itmnam = vec_itmnam(I)

c_______Check type of data item is real
        btype = 3
        call ccp4ccif_check_type(itmnam, btype, sline)

        istat = keep_context
        call ccif_get_real(itmnam, val, rval, ncntxt, istat)
        if (istat.eq.loop_value.or.istat.eq.single_value) then
          RO(I,4) = rval
        else
          RO(I,4) = 0.0
        endif

      ENDDO

      RO(4,1) = 0.0
      RO(4,2) = 0.0
      RO(4,3) = 0.0
      RO(4,4) = 1.0

      CALL CCIF_RELEASE_CONTEXT(NCNTXT)

      return
      end

      subroutine ccp4ccif_getfracmat(blk_id,RF,IFAIL)

CDOC  Get fractionalising matrix for given data block.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  RF(4,4)         (O)     REAL            fractionalising matrix
CDOC
CDOC  IFAIL           (O)     INTEGER         =0 OK
CDOC                                          =1 not found in file

      IMPLICIT NONE

      include 'cciflib.fh'
      
      integer i,J,K,blk_id,ncntxt,istat,
     & btype,sline,IFAIL,istatus
      character*(cfllen) val, blknam, 
     & mat_itmnam(9), vec_itmnam(9), mat_catnam, itmnam
      real RF(4,4),rval

      mat_itmnam(1) = '_atom_sites.Fract_transf_matrix[1][1]'
      mat_itmnam(2) = '_atom_sites.Fract_transf_matrix[1][2]'
      mat_itmnam(3) = '_atom_sites.Fract_transf_matrix[1][3]'
      mat_itmnam(4) = '_atom_sites.Fract_transf_matrix[2][1]'
      mat_itmnam(5) = '_atom_sites.Fract_transf_matrix[2][2]'
      mat_itmnam(6) = '_atom_sites.Fract_transf_matrix[2][3]'
      mat_itmnam(7) = '_atom_sites.Fract_transf_matrix[3][1]'
      mat_itmnam(8) = '_atom_sites.Fract_transf_matrix[3][2]'
      mat_itmnam(9) = '_atom_sites.Fract_transf_matrix[3][3]'
      vec_itmnam(1) = '_atom_sites.Fract_transf_vector[1]'
      vec_itmnam(2) = '_atom_sites.Fract_transf_vector[2]'
      vec_itmnam(3) = '_atom_sites.Fract_transf_vector[3]'
 
C--- Set-up context for ATOM_SITES category
      mat_catnam = 'ATOM_SITES'

C--- ATOM_SITES can appear as items or loop
      istatus = 0
      call ccp4ccif_setup_context(mat_catnam,blk_id,ncntxt,istatus,' ')
      IF (ISTATUS.EQ.cat_not_present) THEN
        IFAIL = 1
        RETURN
      ENDIF

C--- Matrix elements

      K = 0
      DO I=1,3
       DO J=1,3
        K = K + 1

        itmnam = mat_itmnam(k)

c_______Check type of data item is real
        btype = 3
        call ccp4ccif_check_type(itmnam, btype, sline)

        istat = keep_context
        call ccif_get_real(itmnam, val, rval, ncntxt, istat)
        if (istat.eq.loop_value.or.istat.eq.single_value) then
          RF(I,J) = rval
        else
          IFAIL = 1
          RETURN
        endif

       ENDDO
      ENDDO

C--- Vector elements
C    (if matrix was present, but not vector, assume it is zero)

      DO I=1,3

        itmnam = vec_itmnam(I)

c_______Check type of data item is real
        btype = 3
        call ccp4ccif_check_type(itmnam, btype, sline)

        istat = keep_context
        call ccif_get_real(itmnam, val, rval, ncntxt, istat)
        if (istat.eq.loop_value.or.istat.eq.single_value) then
          RF(I,4) = rval
        else
          RF(I,4) = 0.0
        endif

      ENDDO

      RF(4,1) = 0.0
      RF(4,2) = 0.0
      RF(4,3) = 0.0
      RF(4,4) = 1.0

      CALL CCIF_RELEASE_CONTEXT(NCNTXT)

      return
      end

      subroutine ccp4ccif_CELL2MAT(CELL,NCODE,RO,RF)
      
CDOC  Given cell and orthogonalisation code, calculate
CDOC  orthogonalising and fractionalising matrices
CDOC
CDOC  Arguments :
CDOC
CDOC  cell(6)         (I)     REAL            cell dimensions
CDOC
CDOC  NCODE           (I)     INTEGER         orthogonalisation code
CDOC
CDOC  RO(4,4)         (O)     REAL            orthogonalising matrix
CDOC
CDOC  RF(4,4)         (O)     REAL            fractionalising matrix

      IMPLICIT NONE

      INTEGER NCODE
      REAL CELL(6),RO(4,4),RF(4,4)
      real CONV,ALPH,BET,GAMM,
     +  SINA,COSA,SINB,COSB,SING,COSG,A,B,C,
     +  SINAS,COSAS,SINBS,COSBS,SINGS,COSGS

      CONV = ATAN(1.0)*4.0/180.0
      ALPH = CELL(4)*CONV
      BET = CELL(5)*CONV
      GAMM = CELL(6)*CONV
      SINA = SIN(ALPH)
      COSA = COS(ALPH)
      SINB = SIN(BET)
      COSB = COS(BET)
      SING = SIN(GAMM)
      COSG = COS(GAMM)
      COSAS = (COSG*COSB-COSA)/ (SINB*SING)
      SINAS = SQRT(1.0-COSAS*COSAS)
      COSBS = (COSA*COSG-COSB)/ (SINA*SING)
      SINBS = SQRT(1.0-COSBS*COSBS)
      COSGS = (COSA*COSB-COSG)/ (SINA*SINB)
      SINGS = SQRT(1.0-COSGS*COSGS)
      A = CELL(1)
      B = CELL(2)
      C = CELL(3)
C
C---- Calculate RO
C
C---- XO along a  Zo along c*
C
      IF (NCODE.EQ.1) THEN
        RO(1,1) = A
        RO(1,2) = B*COSG
        RO(1,3) = C*COSB
        RO(2,2) = B*SING
        RO(2,3) = -C*SINB*COSAS
        RO(3,3) = C*SINB*SINAS
C
C---- XO along b  Zo along a*
C
      ELSEIF (NCODE.EQ.2) THEN
        RO(1,1) = A*COSG
        RO(1,2) = B
        RO(1,3) = C*COSA
        RO(2,1) = -A*SING*COSBS
        RO(2,3) = C*SINA
        RO(3,1) = A*SING*SINBS
C
C---- XO along c  Zo along b*
C
      ELSEIF (NCODE.EQ.3) THEN
        RO(1,1) = A*COSB
        RO(1,2) = B*COSA
        RO(1,3) = C
        RO(2,1) = A*SINB
        RO(2,2) = -B*SINA*COSGS
        RO(3,2) = B*SINA*SINGS
C
C---- trigonal only - XO along a+b  YO alon a-b  Zo along c*
C
      ELSEIF (NCODE.EQ.4) THEN
        RO(1,1) = A/2.0
        RO(1,2) = A/2.0
        RO(2,1) = -A*SING
        RO(2,2) = A*SING
        RO(3,3) = C
C
C---- XO along a*   ZO along c
C
      ELSEIF (NCODE.EQ.5) THEN
        RO(1,1) = A*SINB*SINGS
        RO(2,1) = -A*SINB*COSGS
        RO(2,2) = B*SINA
        RO(3,1) = A*COSB
        RO(3,2) = B*COSA
        RO(3,3) = C
C
C---- Grr*! to  Gerard Bricogne - his setting for P1 in SKEW.
C     XO along a  Yo along b*
C
      ELSEIF (NCODE.EQ.6) THEN
        RO(1,1) = A
        RO(1,2) = B*COSG
        RO(1,3) = C*COSB
        RO(2,2) = B*SING*SINAS
        RO(3,2) = -B*SING*COSAS
        RO(3,3) = C*SINB
      ENDIF

      RO(4,4)=1.0

C----Now calculate RO,RF from RR.

      CALL ccp4ccif_MAT4INV(RO,RF)

      return
      end

      subroutine ccp4ccif_MAT4INV(A,AI)

CDOC  Subroutine to invert 4*4 matrices for conversion between
CDOC  fractional and orthogonal axes.
CDOC
CDOC  Arguments :
CDOC
CDOC  A(4,4)          (I)     REAL            MATRIX TO BE INVERTED
CDOC
CDOC  AI(4,4)         (O)     REAL            INVERSE MATRIX

      REAL A(4,4),AI(4,4),C(4,4),X(3,3)
C
C---- Get cofactors of 'a' in array 'c'
C
      DO 40 II=1,4
      DO 30 JJ=1,4
      I=0
      DO 20 I1=1,4
      IF(I1.EQ.II)GO TO 20
      I=I+1
      J=0
      DO 10 J1=1,4
      IF(J1.EQ.JJ)GO TO 10
      J=J+1
      X(I,J)=A(I1,J1)
10    CONTINUE
20    CONTINUE
      AM=X(1,1)*X(2,2)*X(3,3)-X(1,1)*X(2,3)*X(3,2)+X(1,2)*X(2,3)*X(3,1)
     *  -X(1,2)*X(2,1)*X(3,3)+X(1,3)*X(2,1)*X(3,2)-X(1,3)*X(2,2)*X(3,1)
      C(II,JJ)=(-1)**(II+JJ)*AM
30    CONTINUE
40    CONTINUE
C
C---- Calculate determinant
C
      D=0
      DO 50 I=1,4
      D=D+A(I,1)*C(I,1)
50    CONTINUE
C
C---- Get inverse matrix
C
      DO 70 I=1,4
      DO 60 J=1,4
      AI(I,J)=C(J,I)/D
60    CONTINUE
70    CONTINUE

      return
      end

      subroutine ccp4ccif_putentity(blk_id,entityid,entitytype,nentity)

CDOC  Put entity information for given data block.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  entityid        (I)     CHARACTER       entity identifier
CDOC
CDOC  entitytype      (I)     CHARACTER       entity type: 'polymer',
CDOC                                          'non-polymer' or 'water'
CDOC
CDOC  NENTITY         (I)     INTEGER         number of entities

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER blk_id,NENTITY
      character*(*) entityid(*),entitytype(*)
      
      integer i, ncntxt,istat,lenval,
     & btype,sline,istatus,ival
      character*(cfllen) val,cval,entity_itmnam(2),entity_catnam
      
      entity_itmnam(1) = '_entity.id'
      entity_itmnam(2) = '_entity.type'

c_____Check type of data item is character
      btype = 1
      call ccp4ccif_check_type(entity_itmnam(1), btype, sline)
c_____Check type of data item is character
      btype = 1
      call ccp4ccif_check_type(entity_itmnam(2), btype, sline)
 
C--- Set-up context for ENTITY category
      entity_catnam = 'ENTITY'

      istatus = cat_not_present
      call ccp4ccif_setup_context(entity_catnam,blk_id,ncntxt,
     +                                               istatus,'LOOP')

      DO 10 i = 1,NENTITY

C--- entity id
         cval = entityid(i)
         istat = append_row
         call ccif_put_char(entity_itmnam(1), cval, ncntxt, istat)

C--- entity type
         cval = entitytype(i)
         istat = keep_context
         call ccif_put_char(entity_itmnam(2), cval, ncntxt, istat)

 10   continue

      CALL CCIF_RELEASE_CONTEXT(NCNTXT)

      return
      end

      subroutine ccp4ccif_putaudit(blk_id,update_record)

CDOC  Put audit information for given data block.
CDOC  If there is not yet an audit category for the data block, a new
CDOC  one will be written. This includes simple strings for _audit.revision_id
CDOC  and _audit.creation_method, the current date for _audit.creation_date,
CDOC  and the subroutine argument as _audit.update_record  
CDOC  If an audit category does exist, then the subroutine argument is
CDOC  appended to _audit.update_record while other data items are left alone.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  update_record   (I)     CHARACTER       line of text to be included
CDOC                                          in _audit.update_record

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER blk_id
      character*(*) update_record
      
      integer i, ncntxt,istat,lenval,lenstr,
     & btype,sline,istatus,ival,nline,itmpos
      character*(cfllen) val,cval,audit_itmnam(4),audit_catnam
      character progname*80,ciftime*25
      character*(cftxtlen) txtval
      
      audit_itmnam(1) = '_audit.revision_id'
      audit_itmnam(2) = '_audit.creation_date'
      audit_itmnam(3) = '_audit.creation_method'
      audit_itmnam(4) = '_audit.update_record'

c_____Check type of data item is character or text
      btype = 1
      call ccp4ccif_check_type(audit_itmnam(1), btype, sline)
      btype = 1
      call ccp4ccif_check_type(audit_itmnam(2), btype, sline)
      btype = 1
      call ccp4ccif_check_type(audit_itmnam(3), btype, sline)
      btype = 1
      call ccp4ccif_check_type(audit_itmnam(4), btype, sline)
 
C--- Set-up context for AUDIT category
      audit_catnam = 'AUDIT'

      istatus = cat_not_present
      call ccp4ccif_setup_context(audit_catnam,blk_id,ncntxt,
     +                                               istatus,' ')

C--- audit revision_id
      istat = keep_context
      call ccif_get_char(audit_itmnam(1),val,cval,lenval,ncntxt,istat)
      if (istat.ne.single_value.and.istat.ne.loop_value) then
        cval = '1'
        istat = keep_context
        call ccif_put_char(audit_itmnam(1), cval, ncntxt, istat)
      endif

C--- audit creation_date
      istat = keep_context
      call ccif_get_char(audit_itmnam(2),val,cval,lenval,ncntxt,istat)
      if (istat.ne.single_value.and.istat.ne.loop_value) then
        call Hciftime(cval)
        istat = keep_context
        call ccif_put_char(audit_itmnam(2), cval(1:lenstr(cval)), 
     +         ncntxt, istat)
      endif

C--- audit creation_method
      istat = keep_context
      itmpos = 1
      call ccif_get_text(audit_itmnam(3),itmpos,nline,txtval,
     +                        cftxtlen/80,ncntxt,istat)
      if (istat.ne.single_value.and.istat.ne.loop_value) then
        cval = 'created by CCP4 suite'
        istat = keep_context
        call ccif_put_text(audit_itmnam(3),1,cval,1,ncntxt,istat,' ')
      endif

C--- audit update_record
      CALL Hciftime(ciftime)
      call CCPPNM (PROGNAME)
      cval =  '  '//ciftime(1:10)//'    '//
     +        PROGNAME(1:lenstr(PROGNAME))//': '//
     +        update_record(1:lenstr(update_record))
      istat = keep_context
      call ccif_put_text(audit_itmnam(4),1,cval,1,ncntxt,istat,' ')

      CALL CCIF_RELEASE_CONTEXT(NCNTXT)

      return
      end

      subroutine ccp4ccif_putcell(blk_id,cell)

CDOC  Put cell information for given data block.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         Block ID of data
CDOC
CDOC  cell(6)         (I)     REAL            Cell dimensions.
CDOC                                          Cell volume is derived
CDOC                                          from these.

      IMPLICIT NONE

      include 'cciflib.fh'
      
      integer i, blk_id,ncntxt,istat,
     & btype,sline,IFAIL
      character*(cfllen) cell_itmnam(7), itmnam, cell_catnam
      real cell(6),vol,rval,CONV,ALPH,BET,GAMM,SUM

C--- Derive cell volume
      CONV = ATAN(1.0)*4.0/180.0
      ALPH = CELL(4)*CONV
      BET = CELL(5)*CONV
      GAMM = CELL(6)*CONV
      SUM = (ALPH+BET+GAMM)*0.5
      vol = 2.0*CELL(1)*CELL(2)*CELL(3)*
     +    SQRT(SIN(SUM-ALPH)*SIN(SUM-BET)*SIN(SUM-GAMM)*SIN(SUM))

      cell_itmnam(1) = '_cell.length_a'
      cell_itmnam(2) = '_cell.length_b'
      cell_itmnam(3) = '_cell.length_c'
      cell_itmnam(4) = '_cell.angle_alpha'
      cell_itmnam(5) = '_cell.angle_beta'
      cell_itmnam(6) = '_cell.angle_gamma'
      cell_itmnam(7) = '_cell.volume'
 
C--- Set-up context for CELL category: category may or may not
C    exist already.
      cell_catnam = 'CELL'
      istat = cat_not_present
      call ccp4ccif_setup_context(cell_catnam,blk_id,ncntxt,istat,' ')

C--- Cell lengths and angles

      DO I=1,7

        itmnam = cell_itmnam(I)

c_______Check type of data item (real, integer, etc.)
        btype = 4
        call ccp4ccif_check_type(itmnam, btype, sline)

        istat = keep_context
        IF (I.LE.6) THEN
          rval = cell(I)
        ELSE
          RVAL = VOL
        ENDIF
        call ccif_put_real(itmnam, rval, ncntxt, istat)

      ENDDO

      CALL CCIF_RELEASE_CONTEXT(NCNTXT)

      CALL CCPERR(4,'Cell information written to file.')

      return
      end

      subroutine ccp4ccif_putsymmetry(blk_id,
     +                   NumSpaceGroup,SpaceGroupName)

CDOC  Get symmetry information for given data block.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  NumSpaceGroup   (I)     INTEGER         spacegroup name
CDOC
CDOC  SpaceGroupName  (I)     CHARACTER       spacegroup number
CDOC
CDOC  Only one of NumSpaceGroup,SpaceGroupName need be given.
CDOC  If either is unknown, set it to 0 or ' ' respectively
CDOC  and it will be inferred from the other.

      IMPLICIT NONE

      include 'cciflib.fh'

C-- Arguments
      INTEGER BLK_ID,NumSpaceGroup
      character*(*) SpaceGroupName

      integer i, length_dep,angle_dep,ncntxt,istat,lenval,
     + btype,sline,istate,istatus,ival,NSYMP,NSYM,NSPGP
      REAL ROT(4,4,192)
      character*10 PointGroupName,NAMSPGP
      character*(cfllen) val, esdval, blknam, cval,
     & symm_itmnam(2), symm_catnam, itmnam

      IF (NumSpaceGroup.EQ.0 .AND. SpaceGroupName.EQ.' ') THEN
        WRITE(6,*) ' Warning: no symmetry information passed ',
     +       'to ccp4ccif_putsymmetry!'
        RETURN
      ENDIF

      NSPGP = NumSpaceGroup
      NAMSPGP = SpaceGroupName

C--- Infer NSPGP from NAMSPGP or vice versa.
C    In any case, CIFize NAMSPGP.
      CALL MSYMLB2(24,NSPGP,NAMSPGP,
     +        PointGroupName,NSYMP,NSYM,ROT)

      symm_itmnam(1) = '_symmetry.Int_Tables_number'
      symm_itmnam(2) = '_symmetry.space_group_name_H-M'
 
C--- Set-up context for SYMM category
      symm_catnam = 'SYMMETRY'
      istatus = 0
      call ccp4ccif_setup_context(symm_catnam,blk_id,ncntxt,istatus,' ')

C--- symmetry number

      itmnam = symm_itmnam(1)

c_____Check type of data item (real, integer, etc.)
      btype = 2
      call ccp4ccif_check_type(itmnam, btype, sline)

      istat = keep_context
      ival = NSPGP
      call ccif_put_int(itmnam, ival, ncntxt, istat)

C--- symmetry name

      itmnam = symm_itmnam(2)

c_____Check type of data item (real, integer, etc.)
      btype = 1
      call ccp4ccif_check_type(itmnam, btype, sline)

      istat = keep_context
      cval = NAMSPGP
      call ccif_put_char(itmnam, cval, ncntxt, istat)

      CALL CCIF_RELEASE_CONTEXT(NCNTXT)

      CALL CCPERR(4,'Symmetry information written to file.')

      return
      end

      subroutine ccp4ccif_putfracmat(blk_id,RF)

CDOC  Put fractionalising matrix for given data block.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  RF(4,4)         (I)     REAL            fractionalising matrix
CDOC

      IMPLICIT NONE

      include 'cciflib.fh'
      
      integer i,J,K,blk_id,ncntxt,istat,
     & btype,sline,istatus
      character*(cfllen) val, blknam, 
     & mat_itmnam(9), vec_itmnam(9), mat_catnam, itmnam
      real RF(4,4),rval

      mat_itmnam(1) = '_atom_sites.Fract_transf_matrix[1][1]'
      mat_itmnam(2) = '_atom_sites.Fract_transf_matrix[1][2]'
      mat_itmnam(3) = '_atom_sites.Fract_transf_matrix[1][3]'
      mat_itmnam(4) = '_atom_sites.Fract_transf_matrix[2][1]'
      mat_itmnam(5) = '_atom_sites.Fract_transf_matrix[2][2]'
      mat_itmnam(6) = '_atom_sites.Fract_transf_matrix[2][3]'
      mat_itmnam(7) = '_atom_sites.Fract_transf_matrix[3][1]'
      mat_itmnam(8) = '_atom_sites.Fract_transf_matrix[3][2]'
      mat_itmnam(9) = '_atom_sites.Fract_transf_matrix[3][3]'
      vec_itmnam(1) = '_atom_sites.Fract_transf_vector[1]'
      vec_itmnam(2) = '_atom_sites.Fract_transf_vector[2]'
      vec_itmnam(3) = '_atom_sites.Fract_transf_vector[3]'
 
C--- Set-up context for ATOM_SITES category: category may or may not
C    exist already.
      mat_catnam = 'ATOM_SITES'
      istatus = 0
      call ccp4ccif_setup_context(mat_catnam,
     +                          blk_id,ncntxt,istatus,'LOOP')

      K = 0
      DO I=1,3

C--- Matrix elements
       DO J=1,3
        K = K + 1

        itmnam = mat_itmnam(k)

c_______Check type of data item is real
        btype = 3
        call ccp4ccif_check_type(itmnam, btype, sline)

        istat = keep_context
        if (K.EQ.1) istat = append_row
        rval = RF(I,J)
        call ccif_put_real(itmnam, rval, ncntxt, istat)

       ENDDO

C--- Vector elements
        itmnam = vec_itmnam(I)

c_______Check type of data item is real
        btype = 3
        call ccp4ccif_check_type(itmnam, btype, sline)

        istat = keep_context
        rval = RF(I,4)
        call ccif_put_real(itmnam, rval, ncntxt, istat)

      ENDDO

      CALL CCIF_RELEASE_CONTEXT(NCNTXT)

      return
      end

C-----------------------------------------------------------------------
C     Routines below here are high-level get/put routines
C     which call lower-level routines. These are compound
C     get/put routines for reading/writing rows or columns.

      subroutine ccp4ccif_getatominfo(blk_id,ATOMNO,ATOMID,ALTID,
     + RESID,CHAINID,RESNO,symbol,IRES,x,biso,u_aniso,occup,IFAIL,LEND)
      
CDOC  Given a block_id, get the next set of atom info.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  ATOMNO          (O)     CHARACTER       '_atom_site.id'
CDOC
CDOC  ATOMID          (O)     CHARACTER       '_atom_site.label_atom_id'
CDOC
CDOC  ALTID           (O)     CHARACTER       '_atom_site.label_alt_id'
CDOC
CDOC  RESID           (O)     CHARACTER       '_atom_site.label_comp_id'
CDOC
CDOC  CHAINID         (O)     CHARACTER       '_atom_site.label_asym_id'
CDOC
CDOC  RESNO           (O)     CHARACTER       '_atom_site.auth_seq_id'
CDOC
CDOC  SYMBOL          (O)     CHARACTER       '_atom_site.type_symbol'
CDOC
CDOC  IRES            (O)     INTEGER         '_atom_site.label_seq_id'
CDOC
CDOC  X               (O)     REAL            array dimension 3 with x,y,z
CDOC
CDOC  BISO            (O)     REAL            isotropic B factor
CDOC
CDOC  U_aniso(6)      (O)     REAL            anisotropic U factor
CDOC
CDOC  OCCUP           (O)     REAL            occupancy
CDOC
CDOC  IFAIL(12)       (O)     INTEGER         array of IFAILs for arguments
CDOC                                          =  0 value specified in CIF returned
CDOC                                          =  1 dictionary default returned
CDOC                                          = -1 '.' returned
CDOC                                          = -2 nothing or '?' returned
CDOC
CDOC  LEND            (O)     LOGICAL         .TRUE. if end of loop

      IMPLICIT NONE

      include 'cciflib.fh'

C---data items
      CHARACTER*(*) SYMBOL,ATOMID,RESID,CHAINID,ALTID,RESNO,ATOMNO
      INTEGER IRES
      REAL X(3),biso,u_aniso(6),occup

      INTEGER I,BLK_ID,ncntxt,istat, btype, sline,istate,
     +  istatus,IFAIL(12)
      character*(cfllen) val,esdval,atom_site_catnam,itmnam,
     +  ATOM_SITE_ITMNAM(3)
      REAL rval,esd
      LOGICAL LEND
      
C--- Has an ATOM_SITE context been set for this data block?
      IF (IATOM_CONTEXT(blk_id).EQ.-1) THEN
        atom_site_catnam = 'ATOM_SITE'
        istatus = loop_context
        call ccp4ccif_setup_context(atom_site_catnam,blk_id,ncntxt, 
     +                istatus,' ')
        IATOM_CONTEXT(blk_id) = ncntxt
        istat = keep_context
      ELSE
        NCNTXT = IATOM_CONTEXT(blk_id)
        istat = IATOM_STATUS(blk_id)
        IF (ISTAT.EQ.0) istat = advance_context
      ENDIF

      IATOM_STATUS(blk_id) = ISTAT

      CALL ccp4ccif_getatomlabels(blk_id,ATOMNO,ATOMID,ALTID,
     +   RESID,CHAINID,RESNO,symbol,IRES,IFAIL,LEND)
      IF (LEND) GOTO 100
      IATOM_STATUS(blk_id) = keep_context
      CALL ccp4ccif_getcoord(blk_id,x,IFAIL(9),LEND)
      IF (LEND) GOTO 100
      IATOM_STATUS(blk_id) = keep_context
      CALL ccp4ccif_getbiso(blk_id,biso,IFAIL(10),LEND)
      IF (LEND) GOTO 100
      IATOM_STATUS(blk_id) = keep_context
      CALL ccp4ccif_getanisou(blk_id,u_aniso,IFAIL(11),LEND)
      IF (LEND) GOTO 100
      IATOM_STATUS(blk_id) = keep_context
      CALL ccp4ccif_getocc(blk_id,occup,IFAIL(12),LEND)

 100  CONTINUE

      IATOM_STATUS(blk_id) = 0

      return
      end

      subroutine ccp4ccif_putatominfo(blk_id,ATOMNO,ATOMID,ALTID,
     +   RESID,CHAINID,RESNO,SYMBOL,IRES,x,biso,occup,NEWROW,IFAIL)
      
CDOC  Given a block_id, put the next set of atom info.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  ATOMNO          (I)     CHARACTER       '_atom_site.id'
CDOC
CDOC  ATOMID          (I)     CHARACTER       '_atom_site.label_atom_id'
CDOC
CDOC  ALTID           (I)     CHARACTER       '_atom_site.label_alt_id'
CDOC
CDOC  RESID           (I)     CHARACTER       '_atom_site.label_comp_id'
CDOC
CDOC  CHAINID         (I)     CHARACTER       '_atom_site.label_asym_id'
CDOC
CDOC  RESNO           (I)     CHARACTER       '_atom_site.auth_seq_id'
CDOC
CDOC  SYMBOL          (I)     CHARACTER       '_atom_site.type_symbol'
CDOC
CDOC  IRES            (I)     INTEGER         '_atom_site.label_seq_id'
CDOC
CDOC  X               (I)     REAL            array dimension 3 with x,y,z
CDOC
CDOC  BISO            (I)     REAL            isotropic B factor
CDOC
CDOC  OCCUP           (I)     REAL            occupancy
CDOC
CDOC  NEWROW          (I)     LOGICAL         .TRUE. if data for new row
CDOC                                          .FALSE. if for existing row
CDOC
CDOC  IFAIL(13)       (I)     INTEGER         =  0 OK
CDOC                                          = -1 write out '?'
CDOC                                          = -2 write out '.'

      IMPLICIT NONE

      include 'cciflib.fh'

C---data items
      CHARACTER*(*) SYMBOL,ATOMID,RESID,CHAINID,ALTID,RESNO,ATOMNO
      INTEGER IRES
      REAL X(3),biso,occup

      INTEGER I,BLK_ID,ncntxt,istat, btype, sline,istate,IFAIL(13),
     +  istatus
      character*(cfllen) val,esdval,atom_site_catnam,itmnam,
     +  ATOM_SITE_ITMNAM(3),disposition
      REAL rval,esd
      LOGICAL NEWROW
   
C--- Has an ATOM_SITE context been set for this data block?
C    Category may or may not exist already.
      IF (IATOM_CONTEXT(blk_id).EQ.-1) THEN
        atom_site_catnam = 'ATOM_SITE'
        istatus = cat_not_present
        disposition = 'LOOP'
        call ccp4ccif_setup_context(atom_site_catnam,blk_id,ncntxt, 
     +                istatus,disposition)
        IATOM_CONTEXT(blk_id) = ncntxt
      ENDIF

      IF (NEWROW) THEN
        IATOM_STATUS(blk_id) = append_row
      ELSE
        IATOM_STATUS(blk_id) = keep_context
      ENDIF

      CALL ccp4ccif_putatomlabels(blk_id,ATOMNO,ATOMID,ALTID,
     +   RESID,CHAINID,RESNO,SYMBOL,IRES,NEWROW,IFAIL)
      IATOM_STATUS(blk_id) = keep_context
      CALL ccp4ccif_putcoord(blk_id,x,NEWROW,IFAIL(9))
      IATOM_STATUS(blk_id) = keep_context
      CALL ccp4ccif_putbiso(blk_id,biso,NEWROW,IFAIL(12))
      IATOM_STATUS(blk_id) = keep_context
      CALL ccp4ccif_putocc(blk_id,occup,NEWROW,IFAIL(13))

      IATOM_STATUS(blk_id) = 0

      return
      end

      subroutine ccp4ccif_getallcoord(blk_id,x,y,z,npack,IFAIL,LEND)
      
CDOC  Given a block_id, get the next npack sets of atom coords.
CDOC  Reading of atomic coordinates will abort if the end of the loop
CDOC  is encountered, or if the coordinates are missing from a row.
CDOC  On exit, NPACK gives the number of coordinate sets actually read.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  X               (O)     REAL            X coordinates with array 
CDOC                                          dimension of at least NPACK
CDOC
CDOC  Y               (O)     REAL            Y coordinates with array 
CDOC                                          dimension of at least NPACK
CDOC
CDOC  Z               (O)     REAL            Z coordinates with array 
CDOC                                          dimension of at least NPACK
CDOC
CDOC  NPACK           (I/O)   INTEGER         Number of coordinates to read.
CDOC                                          On output, number actually read.
CDOC
CDOC  IFAIL           (O)     INTEGER         =  0 value specified in CIF returned
CDOC                                          =  1 dictionary default returned
CDOC                                          = -1 '.' returned
CDOC                                          = -2 nothing or '?' returned
CDOC
CDOC  LEND            (O)     LOGICAL         .TRUE. if end of loop

      IMPLICIT NONE

      include 'cciflib.fh'

C---data items
      REAL XONE(3),X(*),Y(*),Z(*)

      INTEGER I,BLK_ID,ncntxt,istat, btype, sline,istate,IFAIL,
     +  istatus,npack,nread
      character*(cfllen) val,esdval,atom_site_catnam,itmnam,
     +  ATOM_SITE_ITMNAM(3)
      REAL rval,esd
      LOGICAL LEND
      
C--- Has an ATOM_SITE context been set for this data block?
      IF (IATOM_CONTEXT(blk_id).EQ.-1) THEN
        atom_site_catnam = 'ATOM_SITE'
        istatus = loop_context
        call ccp4ccif_setup_context(atom_site_catnam,blk_id,ncntxt, 
     +                istatus,' ')
        IATOM_CONTEXT(blk_id) = ncntxt
        istat = keep_context
      ELSE
        NCNTXT = IATOM_CONTEXT(blk_id)
        istat = IATOM_STATUS(blk_id)
        IF (ISTAT.EQ.0) istat = advance_context
      ENDIF

      IATOM_STATUS(blk_id) = ISTAT

      NREAD = 0
      DO I=1,NPACK

        CALL ccp4ccif_getcoord(blk_id,XONE,IFAIL,LEND)
        IF (LEND) GOTO 100
        IF (IFAIL.LT.0) GOTO 100
        NREAD = NREAD + 1
        IATOM_STATUS(blk_id) = advance_context

        X(I) = XONE(1)
        Y(I) = XONE(2)
        Z(I) = XONE(3)

      ENDDO

 100  CONTINUE

C---Return number actually read.
      NPACK = NREAD

      IATOM_STATUS(blk_id) = 0

      return
      end

C-----------------------------------------------------------------------
C     Routines below here low-level get/put routines
C     which call CCIF routines.

      subroutine ccp4ccif_getcoord(blk_id,x,IFAIL,LEND)
CC  change this to GetAtomCoord
 
CDOC  Given a block_id, get the next set of x, y, z coords.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  X(3)            (O)     REAL            X,Y,Z coordinates
CDOC
CDOC  IFAIL           (O)     INTEGER         =  0 value specified in CIF returned
CDOC                                          =  1 dictionary default returned
CDOC                                          = -1 '.' returned
CDOC                                          = -2 nothing or '?' returned
CDOC
CDOC  LEND            (O)     LOGICAL         .TRUE. if end of loop

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER I,BLK_ID,ncntxt,istat, btype, sline,istate,IFAIL,
     +  istatus
      character*(cfllen) val,esdval,atom_site_catnam,itmnam,
     +  ATOM_SITE_ITMNAM(3)
      REAL X(3),rval,esd
      LOGICAL LEND
      
      LEND = .FALSE.
      
C--- Has an ATOM_SITE context been set for this data block?
      IF (IATOM_CONTEXT(blk_id).EQ.-1) THEN
        atom_site_catnam = 'ATOM_SITE'
        istatus = loop_context
        call ccp4ccif_setup_context(atom_site_catnam,blk_id,ncntxt, 
     +                istatus,' ')
        IATOM_CONTEXT(blk_id) = ncntxt
        istat = keep_context
      ELSE
        NCNTXT = IATOM_CONTEXT(blk_id)
        istat = IATOM_STATUS(blk_id)
        IF (ISTAT.EQ.0) istat = advance_context
      ENDIF

      ATOM_SITE_ITMNAM(1) = '_atom_site.Cartn_x'
      ATOM_SITE_ITMNAM(2) = '_atom_site.Cartn_y'
      ATOM_SITE_ITMNAM(3) = '_atom_site.Cartn_z'
      IFAIL = -2

      DO I=1,3

        itmnam = ATOM_SITE_ITMNAM(I)

c_______Check type of data item (real, integer, etc.)
        btype = 4
        call ccp4ccif_check_type(itmnam, btype, sline)

        if (i.gt.1) istat = keep_context
        call ccif_get_real_esd(itmnam, val, rval, esdval, esd, 
     &      ncntxt, istat, istate)

C----Did we reach end of loop?
        IF (ISTAT.EQ.end_of_context) THEN
          LEND = .TRUE.
          GOTO 100
        ENDIF

        IFAIL = IFAILSTAT(ISTAT/2)
        IF (IFAIL.GE.0) THEN
          X(I) = RVAL
        ELSE
          X(1) = -999.0
          X(2) = -999.0
          X(3) = -999.0
          GOTO 100
        ENDIF

      ENDDO

 100  CONTINUE

      IATOM_STATUS(blk_id) = 0

      return
      end

      subroutine ccp4ccif_cvfrac_mat(X,Y,Z,XX,YY,ZZ,npack,RR)
 
CDOC  Use a supplied matrix to convert NPACK sets of coordinates
CDOC  from orthogonal to fractional, or vice versa.
CDOC
CDOC  Arguments :
CDOC
CDOC  X               (I)     REAL            input X coordinates with array 
CDOC                                          dimension of at least NPACK
CDOC
CDOC  Y               (I)     REAL            input Y coordinates with array 
CDOC                                          dimension of at least NPACK
CDOC
CDOC  Z               (I)     REAL            input Z coordinates with array 
CDOC                                          dimension of at least NPACK
CDOC
CDOC  XX              (O)     REAL            output X coordinates with array 
CDOC                                          dimension of at least NPACK
CDOC
CDOC  YY              (O)     REAL            output Y coordinates with array 
CDOC                                          dimension of at least NPACK
CDOC
CDOC  ZZ              (O)     REAL            output Z coordinates with array 
CDOC                                          dimension of at least NPACK
CDOC
CDOC  NPACK           (I/O)   INTEGER         Number of coordinate sets to convert.
CDOC                                          On output, number actually converted.
CDOC
CDOC  RR(4,4)         (I)     REAL            fractionalising/orthogonalising  
CDOC                                          matrix as appropriate
CDOC

      INTEGER NPACK,IC
      REAL X(*),Y(*),Z(*),XX(*),YY(*),ZZ(*)
      REAL RR(4,4)

      DO IC = 1,NPACK
         XX(IC)=RR(1,1)*X(IC) + RR(1,2)*Y(IC) +RR(1,3)*Z(IC) +RR(1,4)
         YY(IC)=RR(2,1)*X(IC) + RR(2,2)*Y(IC) +RR(2,3)*Z(IC) +RR(2,4)
         ZZ(IC)=RR(3,1)*X(IC) + RR(3,2)*Y(IC) +RR(3,3)*Z(IC) +RR(3,4)
      ENDDO

      return
      end

      subroutine ccp4ccif_cvfrac_block(blk_id,X,Y,Z,XX,YY,ZZ,
     +                                               npack,IFLAG)
 
CDOC  Given a block_id, convert NPACK sets of coordinates
CDOC  from orthogonal to fractional, or vice versa.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  X               (I)     REAL            input X coordinates with array 
CDOC                                          dimension of at least NPACK
CDOC
CDOC  Y               (I)     REAL            input Y coordinates with array 
CDOC                                          dimension of at least NPACK
CDOC
CDOC  Z               (I)     REAL            input Z coordinates with array 
CDOC                                          dimension of at least NPACK
CDOC
CDOC  XX              (O)     REAL            output X coordinates with array 
CDOC                                          dimension of at least NPACK
CDOC
CDOC  YY              (O)     REAL            output Y coordinates with array 
CDOC                                          dimension of at least NPACK
CDOC
CDOC  ZZ              (O)     REAL            output Z coordinates with array 
CDOC                                          dimension of at least NPACK
CDOC
CDOC  NPACK           (I/O)   INTEGER         Number of coordinate sets to convert.
CDOC                                          On output, number actually converted.
CDOC
CDOC  IFLAG           (I)     INTEGER         Flag =0, Convert coordinates from 
CDOC                                                   fractional to orthogonal
CDOC                                               =1, Convert coordinates from 
CDOC                                                   orthogonal to fractional

      INTEGER NPACK,IC,blk_id,IFLAG,IFAIL
      REAL X(*),Y(*),Z(*),XX(*),YY(*),ZZ(*)
      REAL RR(4,4)

      IF (IFLAG.EQ.0) THEN
        CALL ccp4ccif_getorthomat(blk_id,RR,IFAIL)
      ELSEIF (IFLAG.EQ.1) THEN
        CALL ccp4ccif_getfracmat(blk_id,RR,IFAIL)
      ELSE
        CALL CCPERR(1,'ccp4ccif_cvfrac_block: IFLAG must be 0 or 1')
      ENDIF

      DO IC = 1,NPACK
         XX(IC)=RR(1,1)*X(IC) + RR(1,2)*Y(IC) +RR(1,3)*Z(IC) +RR(1,4)
         YY(IC)=RR(2,1)*X(IC) + RR(2,2)*Y(IC) +RR(2,3)*Z(IC) +RR(2,4)
         ZZ(IC)=RR(3,1)*X(IC) + RR(3,2)*Y(IC) +RR(3,3)*Z(IC) +RR(3,4)
      ENDDO

      return
      end

      subroutine ccp4ccif_getatomlabels(blk_id,ATOMNO,ATOMID,ALTID,
     +   RESID,CHAINID,RESNO,symbol,IRES,IFAIL,LEND)
      
CDOC  Given a block_id, get the next set of atom labels.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  ATOMNO          (O)     CHARACTER       '_atom_site.id'
CDOC
CDOC  ATOMID          (O)     CHARACTER       '_atom_site.label_atom_id'
CDOC
CDOC  ALTID           (O)     CHARACTER       '_atom_site.label_alt_id'
CDOC
CDOC  RESID           (O)     CHARACTER       '_atom_site.label_comp_id'
CDOC
CDOC  CHAINID         (O)     CHARACTER       '_atom_site.label_asym_id'
CDOC
CDOC  RESNO           (O)     CHARACTER       '_atom_site.auth_seq_id'
CDOC
CDOC  SYMBOL          (O)     CHARACTER       '_atom_site.type_symbol'
CDOC
CDOC  IRES            (O)     INTEGER         '_atom_site.label_seq_id'
CDOC
CDOC  IFAIL(8)        (O)     INTEGER         array of IFAILs for arguments
CDOC                                          =  0 value specified in CIF returned
CDOC                                          =  1 dictionary default returned
CDOC                                          = -1 '.' returned
CDOC                                          = -2 nothing or '?' returned
CDOC
CDOC  LEND            (O)     LOGICAL         .TRUE. if end of loop

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER NITEMS
      PARAMETER (NITEMS=8)

C---data items
      CHARACTER*(*) SYMBOL,ATOMID,RESID,CHAINID,ALTID,RESNO,ATOMNO
      INTEGER IRES

      INTEGER I,BLK_ID,ncntxt,istat, btype, sline,istate,
     +  istatus,lenval,ival,ivalue(NITEMS),IFAIL(NITEMS)
      character*(cfllen) val,cval,esdval,atom_site_catnam,
     +  itmnam,ATOM_SITE_ITMNAM(NITEMS),cvalue(NITEMS)
      REAL rval,esd
      LOGICAL LEND
      
      LEND = .FALSE.
      
C--- Has an ATOM_SITE context been set for this data block?
      IF (IATOM_CONTEXT(blk_id).EQ.-1) THEN
        atom_site_catnam = 'ATOM_SITE'
        istatus = loop_context
        call ccp4ccif_setup_context(atom_site_catnam,blk_id,ncntxt, 
     +                istatus,' ')
        IATOM_CONTEXT(blk_id) = ncntxt
        istat = keep_context
      ELSE
        NCNTXT = IATOM_CONTEXT(blk_id)
        istat = IATOM_STATUS(blk_id)
        IF (ISTAT.EQ.0) istat = advance_context
      ENDIF

C--- Get character items

      ATOM_SITE_ITMNAM(1) = '_atom_site.id'
      ATOM_SITE_ITMNAM(2) = '_atom_site.label_atom_id'
      ATOM_SITE_ITMNAM(3) = '_atom_site.label_alt_id'
      ATOM_SITE_ITMNAM(4) = '_atom_site.label_comp_id'
      ATOM_SITE_ITMNAM(5) = '_atom_site.label_asym_id'
      ATOM_SITE_ITMNAM(6) = '_atom_site.auth_seq_id'
      ATOM_SITE_ITMNAM(7) = '_atom_site.type_symbol'

      DO I=1,7

        itmnam = ATOM_SITE_ITMNAM(I)
        IFAIL(I) = -2

c_______Check type of data item (real, integer, etc.)
        btype = 1
        call ccp4ccif_check_type(itmnam, btype, sline)

        if (i.gt.1) istat = keep_context
        call ccif_get_char(itmnam, val, cval, lenval,
     &      ncntxt, istat)

C----Did we reach end of loop?
        IF (ISTAT.EQ.end_of_context) THEN
          LEND = .TRUE.
          GOTO 100
        ENDIF

        IFAIL(I) = IFAILSTAT(ISTAT/2)
        IF (IFAIL(I).GE.0) THEN
          CVALUE(I) = CVAL(1:LENVAL)
        ELSE
          CVALUE(I) = ' '
        ENDIF

      ENDDO

      ATOMNO = CVALUE(1)
      ATOMID = CVALUE(2)
      ALTID = CVALUE(3)
      RESID = CVALUE(4)
      CHAINID = CVALUE(5)
      RESNO = CVALUE(6)
      SYMBOL = CVALUE(7)

C--- Get integer items

      ATOM_SITE_ITMNAM(8) = '_atom_site.label_seq_id'

      DO I=8,8

        itmnam = ATOM_SITE_ITMNAM(I)
        IFAIL(I) = -2

c_______Check type of data item (real, integer, etc.)
        btype = 2
        call ccp4ccif_check_type(itmnam, btype, sline)

        istat = keep_context
        call ccif_get_int(itmnam, val, ival, ncntxt, istat)

C----Did we reach end of loop?
        IF (ISTAT.EQ.8) THEN
          LEND = .TRUE.
          GOTO 100
        ENDIF

        IFAIL(I) = IFAILSTAT(ISTAT/2)
        IF (IFAIL(I).GE.0) THEN
          IVALUE(I) = IVAL
        ELSE
          IVALUE(I) = -999
        ENDIF

      ENDDO

      IRES = IVALUE(8)

 100  CONTINUE

      IATOM_STATUS(blk_id) = 0

      return
      end

      subroutine ccp4ccif_getbiso(blk_id,biso,IFAIL,LEND)
      
CDOC  Given a block_id, get the next isotropic B factor.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  BISO            (O)     REAL            isotropic B factor
CDOC
CDOC  IFAIL           (O)     INTEGER         =  0 value specified in CIF returned
CDOC                                          =  1 dictionary default returned
CDOC                                          = -1 '.' returned
CDOC                                          = -2 nothing or '?' returned
CDOC
CDOC  LEND            (O)     LOGICAL         .TRUE. if end of loop

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER I,BLK_ID,ncntxt,istat, btype, sline,istate,IFAIL,
     +  istatus
      character*(cfllen) val,esdval,atom_site_catnam,itmnam
      REAL BISO,rval,esd
      LOGICAL LEND
      
      LEND = .FALSE.

C--- Has an ATOM_SITE context been set for this data block?
      IF (IATOM_CONTEXT(blk_id).EQ.-1) THEN
        atom_site_catnam = 'ATOM_SITE'
        istatus = loop_context
        call ccp4ccif_setup_context(atom_site_catnam,blk_id,ncntxt, 
     +                istatus,' ')
        IATOM_CONTEXT(blk_id) = ncntxt
        istat = keep_context
      ELSE
        NCNTXT = IATOM_CONTEXT(blk_id)
        istat = IATOM_STATUS(blk_id)
        IF (ISTAT.EQ.0) istat = advance_context
      ENDIF

      itmnam = '_atom_site.B_iso_or_equiv'
      IFAIL = -2

c_____Check type of data item: real with esd
      btype = 4
      call ccp4ccif_check_type(itmnam, btype, sline)

      call ccif_get_real_esd(itmnam, val, rval, esdval, esd, 
     &    ncntxt, istat, istate)

C----Did we reach end of loop?
      IF (ISTAT.EQ.end_of_context) THEN
        LEND = .TRUE.
      ELSE
        IFAIL = IFAILSTAT(ISTAT/2)
        IF (IFAIL.GE.0) THEN
          BISO = RVAL
        ELSE
          BISO = -999.0
        ENDIF
      ENDIF

      IATOM_STATUS(blk_id) = 0

      return
      end

      subroutine ccp4ccif_getanisou(blk_id,u_aniso,IFAIL,LEND)
      
CDOC  Given a block_id, get the set of ADPs for the next
CDOC  atom_site packet. First check the ATOM_SITE_ANISOTROP
CODC  category, if present, for U then B. If not found, try
CDOC  the atom_site packet itself, for U then B.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  U_ANISO(6)      (O)     REAL            anisotropic U factor
CDOC
CDOC  IFAIL           (O)     INTEGER         =  0 value specified in CIF returned
CDOC                                          =  1 dictionary default returned
CDOC                                          = -1 '.' returned
CDOC                                          = -2 nothing or '?' returned
CDOC
CDOC  LEND            (O)     LOGICAL         .TRUE. if end of loop

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER I,BLK_ID,ncntxt,istat, btype, sline,istate,IFAIL,
     +  istatus,IKEY,lenval,istatold
      character*(cfllen) val,esdval,atom_site_catnam,itmnam,
     +  ATOM_SITE_ITMNAM(6),cval
      REAL U_aniso(6),rval,esd,eightpipi,FKEY
      LOGICAL LEND
      
      LEND = .FALSE.
      eightpipi = 128.0*(atan(1.0))**2

C--- Has an ATOM_SITE context been set for this data block?
      IF (IATOM_CONTEXT(blk_id).EQ.-1) THEN
        atom_site_catnam = 'ATOM_SITE'
        istatus = loop_context
        call ccp4ccif_setup_context(atom_site_catnam,blk_id,ncntxt, 
     +                istatus,' ')
        IATOM_CONTEXT(blk_id) = ncntxt
        istat = keep_context
      ELSE
        NCNTXT = IATOM_CONTEXT(blk_id)
        istat = IATOM_STATUS(blk_id)
        IF (ISTAT.EQ.0) istat = advance_context
      ENDIF
      istatold = istat

C   ATOM_SITE_ANISOTROP category, if present, takes precedence
      IF (LANISOTROP(blk_id)) THEN

        itmnam = '_atom_site.id'
        call ccif_get_char(itmnam, val, cval, lenval,
     &      ncntxt, istat)
        IF (ISTAT.NE.loop_value) GOTO 300
        IKEY = 0
        FKEY = 0.0
        CALL CCIF_SEARCH_CONTEXT(IANISOTROP_SORT(blk_id),
     +          IANISOTROP_CONTEXT(blk_id),1,IKEY,FKEY,CVAL,
     +                CFLLEN,ISTAT)
        IF (ISTAT.NE.1) GOTO 300

C  We have now found the current _atom_site.id in ATOM_SITE_ANISOTROP

        ATOM_SITE_ITMNAM(1) = '_atom_site_anisotrop.U[1][1]'
        ATOM_SITE_ITMNAM(2) = '_atom_site_anisotrop.U[1][2]'
        ATOM_SITE_ITMNAM(3) = '_atom_site_anisotrop.U[1][3]'
        ATOM_SITE_ITMNAM(4) = '_atom_site_anisotrop.U[2][2]'
        ATOM_SITE_ITMNAM(5) = '_atom_site_anisotrop.U[2][3]'
        ATOM_SITE_ITMNAM(6) = '_atom_site_anisotrop.U[3][3]'

        DO I=1,6
          itmnam = ATOM_SITE_ITMNAM(I)
c___Check type of data item (real, integer, etc.)
          btype = 4
          call ccp4ccif_check_type(itmnam, btype, sline)
          istat = keep_context
          call ccif_get_real_esd(itmnam, val, rval, esdval, esd, 
     &        IANISOTROP_CONTEXT(blk_id), istat, istate)

C----If no value found, try anisotrop.B
          IF (ISTAT.NE.loop_value) THEN
            GOTO 400
          ENDIF

          IFAIL = IFAILSTAT(ISTAT/2)
          U_aniso(I) = RVAL

        ENDDO

        GOTO 100

 400    CONTINUE

        ATOM_SITE_ITMNAM(1) = '_atom_site_anisotrop.B[1][1]'
        ATOM_SITE_ITMNAM(2) = '_atom_site_anisotrop.B[1][2]'
        ATOM_SITE_ITMNAM(3) = '_atom_site_anisotrop.B[1][3]'
        ATOM_SITE_ITMNAM(4) = '_atom_site_anisotrop.B[2][2]'
        ATOM_SITE_ITMNAM(5) = '_atom_site_anisotrop.B[2][3]'
        ATOM_SITE_ITMNAM(6) = '_atom_site_anisotrop.B[3][3]'

        DO I=1,6
          itmnam = ATOM_SITE_ITMNAM(I)
c___Check type of data item (real, integer, etc.)
          btype = 4
          call ccp4ccif_check_type(itmnam, btype, sline)
          istat = keep_context
          call ccif_get_real_esd(itmnam, val, rval, esdval, esd, 
     &        IANISOTROP_CONTEXT(blk_id), istat, istate)

C----If no value found, try aniso_U
          IF (ISTAT.NE.loop_value) THEN
            GOTO 300
          ENDIF

          IFAIL = IFAILSTAT(ISTAT/2)
          U_aniso(I) = RVAL/eightpipi

        ENDDO

        GOTO 100

      ENDIF

 300  CONTINUE

      istat = istatold
      IFAIL = -2
      ATOM_SITE_ITMNAM(1) = '_atom_site.aniso_U[1][1]'
      ATOM_SITE_ITMNAM(2) = '_atom_site.aniso_U[1][2]'
      ATOM_SITE_ITMNAM(3) = '_atom_site.aniso_U[1][3]'
      ATOM_SITE_ITMNAM(4) = '_atom_site.aniso_U[2][2]'
      ATOM_SITE_ITMNAM(5) = '_atom_site.aniso_U[2][3]'
      ATOM_SITE_ITMNAM(6) = '_atom_site.aniso_U[3][3]'

      DO I=1,6

        itmnam = ATOM_SITE_ITMNAM(I)

c_______Check type of data item (real, integer, etc.)
        btype = 4
        call ccp4ccif_check_type(itmnam, btype, sline)

        if (i.gt.1) istat = keep_context
        call ccif_get_real_esd(itmnam, val, rval, esdval, esd, 
     &      ncntxt, istat, istate)

C----Did we reach end of loop?
        IF (ISTAT.EQ.end_of_context) THEN
          LEND = .TRUE.
          GOTO 100
C----If no value found, try aniso_B
        ELSEIF (ISTAT.NE.loop_value) THEN
          GOTO 200
        ENDIF

        IFAIL = IFAILSTAT(ISTAT/2)
        U_aniso(I) = RVAL

      ENDDO

      GOTO 100

 200  CONTINUE

      IFAIL = -2
      ATOM_SITE_ITMNAM(1) = '_atom_site.aniso_B[1][1]'
      ATOM_SITE_ITMNAM(2) = '_atom_site.aniso_B[1][2]'
      ATOM_SITE_ITMNAM(3) = '_atom_site.aniso_B[1][3]'
      ATOM_SITE_ITMNAM(4) = '_atom_site.aniso_B[2][2]'
      ATOM_SITE_ITMNAM(5) = '_atom_site.aniso_B[2][3]'
      ATOM_SITE_ITMNAM(6) = '_atom_site.aniso_B[3][3]'

      DO I=1,6

        itmnam = ATOM_SITE_ITMNAM(I)

c_______Check type of data item (real, integer, etc.)
        btype = 4
        call ccp4ccif_check_type(itmnam, btype, sline)

        istat = keep_context
        call ccif_get_real_esd(itmnam, val, rval, esdval, esd, 
     &      ncntxt, istat, istate)

C----Did we reach end of loop?
        IF (ISTAT.EQ.end_of_context) THEN
          LEND = .TRUE.
          GOTO 100
        ENDIF

        IFAIL = IFAILSTAT(ISTAT/2)
        IF (IFAIL.GE.0) THEN
          U_aniso(I) = RVAL/eightpipi
        ELSE
          U_aniso(I) = -999.0
        ENDIF

      ENDDO

 100  CONTINUE

      IATOM_STATUS(blk_id) = 0

      return
      end

      subroutine ccp4ccif_getocc(blk_id,occup,IFAIL,LEND)
      
CDOC  Given a block_id, get the next occupancy.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  OCCUP           (O)     REAL            occupancy
CDOC
CDOC  IFAIL           (O)     INTEGER         =  0 value specified in CIF returned
CDOC                                          =  1 dictionary default returned
CDOC                                          = -1 '.' returned
CDOC                                          = -2 nothing or '?' returned
CDOC
CDOC  LEND            (O)     LOGICAL         .TRUE. if end of loop

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER I,BLK_ID,ncntxt,istat, btype, sline,istate,IFAIL,
     +  istatus
      character*(cfllen) val,esdval,atom_site_catnam,itmnam
      REAL OCCUP,rval,esd
      LOGICAL LEND
      
      LEND = .FALSE.
      
C--- Has an ATOM_SITE context been set for this data block?
      IF (IATOM_CONTEXT(blk_id).EQ.-1) THEN
        atom_site_catnam = 'ATOM_SITE'
        istatus = loop_context
        call ccp4ccif_setup_context(atom_site_catnam,blk_id,ncntxt, 
     +                istatus,' ')
        IATOM_CONTEXT(blk_id) = ncntxt
        istat = keep_context
      ELSE
        NCNTXT = IATOM_CONTEXT(blk_id)
        istat = IATOM_STATUS(blk_id)
        IF (ISTAT.EQ.0) istat = advance_context
      ENDIF

      itmnam = '_atom_site.occupancy'
      IFAIL = -2

c_____Check type of data item: real with esd
      btype = 4
      call ccp4ccif_check_type(itmnam, btype, sline)

      call ccif_get_real_esd(itmnam, val, rval, esdval, esd, 
     +    ncntxt, istat, istate)

C----Did we reach end of loop?
      IF (ISTAT.EQ.end_of_context) THEN
        LEND = .TRUE.
      ELSE
        IFAIL = IFAILSTAT(ISTAT/2)
        IF (IFAIL.GE.0) THEN
          OCCUP = RVAL
        ELSE
          OCCUP = -999.0
        ENDIF
      ENDIF

      IATOM_STATUS(blk_id) = 0

      return
      end

      subroutine ccp4ccif_putcoord(blk_id,x,NEWROW,IFAIL)
      
CDOC  Given a block_id, put a set of x, y, z coords.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  X(3)            (I)     REAL            x,y,z coordinates
CDOC
CDOC  NEWROW          (I)     LOGICAL         .TRUE. if data for new row
CDOC                                          .FALSE. if for existing row
CDOC
CDOC  IFAIL(3)        (I)     INTEGER         =  0 OK
CDOC                                          = -1 write out '?'
CDOC                                          = -2 write out '.'

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER I,BLK_ID,ncntxt,istat, btype, sline,istate,IFAIL(3),
     +  istatus
      character*(cfllen) val,atom_site_catnam,itmnam,
     +  disposition,ATOM_SITE_ITMNAM(3)
      REAL X(3),rval
      LOGICAL NEWROW

C--- Has an ATOM_SITE context been set for this data block?
C    Category may or may not exist already.
      IF (IATOM_CONTEXT(blk_id).EQ.-1) THEN
        atom_site_catnam = 'ATOM_SITE'
        istatus = cat_not_present
        disposition = 'LOOP'
        call ccp4ccif_setup_context(atom_site_catnam,blk_id,ncntxt, 
     +                istatus,disposition)
        IATOM_CONTEXT(blk_id) = ncntxt
      ELSE
        NCNTXT = IATOM_CONTEXT(blk_id)
      ENDIF

      ISTAT = IATOM_STATUS(blk_id)
      IF (ISTAT.EQ.0) THEN
        IF (NEWROW) THEN
          istat = append_row
        ELSE
          istat = keep_context
        ENDIF
      ENDIF

      ATOM_SITE_ITMNAM(1) = '_atom_site.Cartn_x'
      ATOM_SITE_ITMNAM(2) = '_atom_site.Cartn_y'
      ATOM_SITE_ITMNAM(3) = '_atom_site.Cartn_z'

      DO I=1,3

        ITMNAM = ATOM_SITE_ITMNAM(I)

c_____Check type of data item: real with esd
        btype = 4
        call ccp4ccif_check_type(itmnam, btype, sline)

        IF (I.GT.1) istat = keep_context

        IF (IFAIL(I).EQ.0) THEN
          rval = X(I)
          call ccif_put_real(itmnam,rval, ncntxt, istat)
C---Can't in general put a char for a real data item, but ? and . are exceptions
        ELSEIF (IFAIL(I).EQ.-1) THEN
          call ccif_put_char(itmnam,'?', ncntxt, istat)
        ELSEIF (IFAIL(I).EQ.-2) THEN
          call ccif_put_char(itmnam,'.', ncntxt, istat)
        ENDIF

      ENDDO

      IATOM_STATUS(blk_id) = 0

      return
      end

      subroutine ccp4ccif_putatomlabels(blk_id,ATOMNO,ATOMID,ALTID,
     +   RESID,CHAINID,RESNO,SYMBOL,IRES,NEWROW,IFAIL)
      
CDOC  Given a block_id, put a set of atom labels.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  ATOMNO          (I)     CHARACTER       '_atom_site.id'
CDOC
CDOC  ATOMID          (I)     CHARACTER       '_atom_site.label_atom_id'
CDOC
CDOC  ALTID           (I)     CHARACTER       '_atom_site.label_alt_id'
CDOC
CDOC  RESID           (I)     CHARACTER       '_atom_site.label_comp_id'
CDOC
CDOC  CHAINID         (I)     CHARACTER       '_atom_site.label_asym_id'
CDOC
CDOC  RESNO           (I)     CHARACTER       '_atom_site.auth_seq_id'
CDOC
CDOC  SYMBOL          (I)     CHARACTER       '_atom_site.type_symbol'
CDOC
CDOC  IRES            (I)     INTEGER         '_atom_site.label_seq_id'
CDOC
CDOC  NEWROW          (I)     LOGICAL         .TRUE. if data for new row
CDOC                                          .FALSE. if for existing row
CDOC
CDOC  IFAIL(8)        (I)     INTEGER         =  0 OK
CDOC                                          = -1 write out '?'
CDOC                                          = -2 write out '.'

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER NITEMS
      PARAMETER (NITEMS=8)

C---data items
      CHARACTER*(*) SYMBOL,ATOMID,RESID,CHAINID,ALTID,RESNO,ATOMNO
      INTEGER IRES

      INTEGER I,BLK_ID,ncntxt,istat, btype, sline,istate,IFAIL0,
     +  IFAIL(NITEMS),istatus,lenval,ival,ivalue(NITEMS)
      character*(cfllen) val,cval,esdval,atom_site_catnam,
     +  disposition,itmnam,ATOM_SITE_ITMNAM(NITEMS),
     +  cvalue(NITEMS)
      REAL rval,esd
      LOGICAL NEWROW

C--- Has an ATOM_SITE context been set for this data block?
C    Category may or may not exist already.
      IF (IATOM_CONTEXT(blk_id).EQ.-1) THEN
        atom_site_catnam = 'ATOM_SITE'
        istatus = cat_not_present
        disposition = 'LOOP'
        call ccp4ccif_setup_context(atom_site_catnam,blk_id,ncntxt, 
     +                istatus,disposition)
        IATOM_CONTEXT(blk_id) = ncntxt
      ELSE
        NCNTXT = IATOM_CONTEXT(blk_id)
      ENDIF

      ISTAT = IATOM_STATUS(blk_id)
      IF (ISTAT.EQ.0) THEN
        IF (NEWROW) THEN
          istat = append_row
        ELSE
          istat = keep_context
        ENDIF
      ENDIF

C--- Put initial 'ATOM' string
      itmnam = '_atom_site.group_PDB'

c_____Check type of data item (real, integer, etc.)
      btype = 1
      call ccp4ccif_check_type(itmnam, btype, sline)

C---If any of arguments are present then IFAIL0 = 0 and we write 'ATOM'
C   This could be customised to a subset of 'essential' arguments.
      IFAIL0 = IFAIL(1)*IFAIL(2)*IFAIL(3)*IFAIL(4)*
     +         IFAIL(5)*IFAIL(6)*IFAIL(7)*IFAIL(8)

      IF (IFAIL0.EQ.0) THEN
        CVAL = 'ATOM'
        CALL CCIF_PUT_CHAR(itmnam, CVAL, ncntxt, istat)
C---Can't in general put a char for a real data item, but ? and . are exceptions
      ELSE
        call ccif_put_char(itmnam,'?', ncntxt, istat)
      ENDIF

C--- Put character items

      ATOM_SITE_ITMNAM(1) = '_atom_site.id'
      ATOM_SITE_ITMNAM(2) = '_atom_site.label_atom_id'
      ATOM_SITE_ITMNAM(3) = '_atom_site.label_alt_id'
      ATOM_SITE_ITMNAM(4) = '_atom_site.label_comp_id'
      ATOM_SITE_ITMNAM(5) = '_atom_site.label_asym_id'
      ATOM_SITE_ITMNAM(6) = '_atom_site.auth_seq_id'
      ATOM_SITE_ITMNAM(7) = '_atom_site.type_symbol'

      CVALUE(1) = ATOMNO
      IF (CVALUE(1).EQ.' ') CVALUE(1) = '?'
      CVALUE(2) = ATOMID
      IF (CVALUE(2).EQ.' ') CVALUE(2) = '?'
      CVALUE(3) = ALTID
      IF (CVALUE(3).EQ.' ') CVALUE(3) = '.'
      CVALUE(4) = RESID
      IF (CVALUE(4).EQ.' ') CVALUE(4) = '?'
      CVALUE(5) = CHAINID
      IF (CVALUE(5).EQ.' ') CVALUE(5) = '?'
      CVALUE(6) = RESNO
      IF (CVALUE(6).EQ.' ') CVALUE(6) = '?'
      CVALUE(7) = SYMBOL
      IF (CVALUE(7).EQ.' ') CVALUE(7) = '?'

      DO I=1,7

        itmnam = ATOM_SITE_ITMNAM(I)

c_______Check type of data item (real, integer, etc.)
        btype = 1
        call ccp4ccif_check_type(itmnam, btype, sline)

        istat = keep_context

        IF (IFAIL(I).EQ.0) THEN
          CVAL = CVALUE(I)
          CALL CCIF_PUT_CHAR(itmnam, CVAL, ncntxt, istat)
C---Can't in general put a char for a real data item, but ? and . are exceptions
        ELSEIF (IFAIL(I).EQ.-1) THEN
          call ccif_put_char(itmnam,'?', ncntxt, istat)
        ELSEIF (IFAIL(I).EQ.-2) THEN
          call ccif_put_char(itmnam,'.', ncntxt, istat)
        ENDIF

      ENDDO

C--- Put integer items

      ATOM_SITE_ITMNAM(8) = '_atom_site.label_seq_id'
      IVALUE(8) = IRES

      DO I=8,8

        itmnam = ATOM_SITE_ITMNAM(I)

c_______Check type of data item (real, integer, etc.)
        btype = 2
        call ccp4ccif_check_type(itmnam, btype, sline)

        istat = keep_context

        IF (IFAIL(I).EQ.0) THEN
          IVAL = IVALUE(I)
          CALL CCIF_PUT_INT(itmnam, IVAL, ncntxt, istat)
C---Can't in general put a char for a real data item, but ? and . are exceptions
        ELSEIF (IFAIL(I).EQ.-1) THEN
          call ccif_put_char(itmnam,'?', ncntxt, istat)
        ELSEIF (IFAIL(I).EQ.-2) THEN
          call ccif_put_char(itmnam,'.', ncntxt, istat)
        ENDIF

      ENDDO

      IATOM_STATUS(blk_id) = 0

      return
      end

      subroutine ccp4ccif_putbiso(blk_id,biso,NEWROW,IFAIL)
      
CDOC  Given a block_id, put an isotropic B factor.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  BISO            (I)     REAL            isotropic B factor
CDOC
CDOC  NEWROW          (I)     LOGICAL         .TRUE. if data for new row
CDOC                                          .FALSE. if for existing row
CDOC
CDOC  IFAIL           (I)     INTEGER         =  0 OK
CDOC                                          = -1 write out '?'
CDOC                                          = -2 write out '.'

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER I,BLK_ID,ncntxt,istat, btype, sline,istate,IFAIL,
     +  istatus
      character*(cfllen) val,atom_site_catnam,itmnam,
     +  disposition
      REAL BISO,rval
      LOGICAL NEWROW

C--- Has an ATOM_SITE context been set for this data block?
C    Category may or may not exist already.
      IF (IATOM_CONTEXT(blk_id).EQ.-1) THEN
        atom_site_catnam = 'ATOM_SITE'
        istatus = cat_not_present
        disposition = 'LOOP'
        call ccp4ccif_setup_context(atom_site_catnam,blk_id,ncntxt, 
     +                istatus,disposition)
        IATOM_CONTEXT(blk_id) = ncntxt
      ELSE
        NCNTXT = IATOM_CONTEXT(blk_id)
      ENDIF

      ISTAT = IATOM_STATUS(blk_id)
      IF (ISTAT.EQ.0) THEN
        IF (NEWROW) THEN
          istat = append_row
        ELSE
          istat = keep_context
        ENDIF
      ENDIF

      itmnam = '_atom_site.B_iso_or_equiv'

c_____Check type of data item: real with esd
      btype = 4
      call ccp4ccif_check_type(itmnam, btype, sline)

      IF (IFAIL.EQ.0) THEN
        rval = BISO
        call ccif_put_real(itmnam,rval, ncntxt, istat)
C---Can't in general put a char for a real data item, but ? and . are exceptions
      ELSEIF (IFAIL.EQ.-1) THEN
        call ccif_put_char(itmnam,'?', ncntxt, istat)
      ELSEIF (IFAIL.EQ.-2) THEN
        call ccif_put_char(itmnam,'.', ncntxt, istat)
      ENDIF

      IATOM_STATUS(blk_id) = 0

      return
      end

      subroutine ccp4ccif_putocc(blk_id,occup,NEWROW,IFAIL)
      
CDOC  Given a block_id, put an occupancy.
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  OCCUP           (I)     REAL            occupancy
CDOC
CDOC  NEWROW          (I)     LOGICAL         .TRUE. if data for new row
CDOC                                          .FALSE. if for existing row
CDOC
CDOC  IFAIL           (I)     INTEGER         =  0 OK
CDOC                                          = -1 write out '?'
CDOC                                          = -2 write out '.'

      IMPLICIT NONE

      include 'cciflib.fh'

      INTEGER I,BLK_ID,ncntxt,istat, btype, sline,istate,IFAIL,
     +  istatus
      character*(cfllen) val,atom_site_catnam,itmnam,
     +  disposition
      REAL OCCUP,rval
      LOGICAL NEWROW

C--- Has an ATOM_SITE context been set for this data block?
C    Category may or may not exist already.
      IF (IATOM_CONTEXT(blk_id).EQ.-1) THEN
        atom_site_catnam = 'ATOM_SITE'
        istatus = cat_not_present
        disposition = 'LOOP'
        call ccp4ccif_setup_context(atom_site_catnam,blk_id,ncntxt, 
     +                istatus,disposition)
        IATOM_CONTEXT(blk_id) = ncntxt
      ELSE
        NCNTXT = IATOM_CONTEXT(blk_id)
      ENDIF

      ISTAT = IATOM_STATUS(blk_id)
      IF (ISTAT.EQ.0) THEN
        IF (NEWROW) THEN
          istat = append_row
        ELSE
          istat = keep_context
        ENDIF
      ENDIF

      itmnam = '_atom_site.occupancy'

c_____Check type of data item: real with esd
      btype = 4
      call ccp4ccif_check_type(itmnam, btype, sline)

      IF (IFAIL.EQ.0) THEN
        rval = OCCUP
        call ccif_put_real(itmnam,rval, ncntxt, istat)
C---Can't in general put a char for a real data item, but ? and . are exceptions
      ELSEIF (IFAIL.EQ.-1) THEN
        call ccif_put_char(itmnam,'?', ncntxt, istat)
      ELSEIF (IFAIL.EQ.-2) THEN
        call ccif_put_char(itmnam,'.', ncntxt, istat)
      ENDIF

      IATOM_STATUS(blk_id) = 0

      return
      end

C-----------------------------------------------------------------------
C     Routines below here are simple wrap-arounds to CCIF routines
C     which include some error checking

      subroutine ccp4ccif_getintdataitem(blk_id,
     +                        catnam,itmnam,iitem,citem,IFAIL)
      
CDOC  Given a block_id, read specified integer data item
CDOC
CDOC  Arguments :
CDOC
CDOC  BLK_ID          (I)     INTEGER         block ID of data
CDOC
CDOC  CATNAM          (I)     CHARACTER       category name
CDOC
CDOC  ITMNAM          (I)     CHARACTER       item name
CDOC
CDOC  IITEM           (I)     INTEGER         item integer value
CDOC
CDOC  CITEM           (I)     CHARACTER       item character value
CDOC
CDOC  IFAIL           (I)     INTEGER         =  0 value specified in CIF returned
CDOC                                          =  1 dictionary default returned
CDOC                                          = -1 '.' returned
CDOC                                          = -2 nothing or '?' returned
CDOC

      IMPLICIT NONE

      include 'cciflib.fh'
      
      integer blk_id,IFAIL,ncntxt,istatus,btype,sline,istat,
     +   ival,iitem
      character*(*) catnam,itmnam,citem
      character*(cfllen) val

      istatus = item_context
      call ccp4ccif_setup_context(catnam,blk_id,ncntxt,istatus,' ')

c_____Check type of data item is integer
      btype = 2
      call ccp4ccif_check_type(itmnam, btype, sline)

      istat = keep_context
      call ccif_get_int(itmnam, val, ival, ncntxt, istat)

      IFAIL = IFAILSTAT(ISTAT/2)
      IF (IFAIL.GE.0) THEN
        IITEM = IVAL
        CITEM = VAL
      ELSE
        IITEM = -999
        CITEM = ' '   
      ENDIF

      CALL CCIF_RELEASE_CONTEXT(NCNTXT)

      return
      end

      subroutine ccp4ccif_setup_context(catnam_in,blk_id,ncntxt, 
     +                istat_exp,disposition)

CDOC  Wrap-around for ccif_setup_context
CDOC
CDOC  catnam_in       (i)     CHARACTER     name of category or item
CDOC
CDOC  blk_id          (i)     INTEGER       block id of data
CDOC
CDOC  ncntxt          (o)     INTEGER       number of assigned context
CDOC
CDOC  istat_exp       (i/o)   INTEGER       On input:
CDOC                                          0 = category not necessarily 
CDOC                                              expected to be present
CDOC                                          1 = loop expected
CDOC                                          2 = item expected
CDOC                                          3 = loop or item expected
CDOC                                        On output:
CDOC                                          0 = category not found
CDOC                                          1 = loop found
CDOC                                          2 = item found
CDOC
CDOC  disposition     (i)     CHARACTER     'RO' or 'LOOP'

      IMPLICIT NONE

      include 'cciflib.fh'

      integer blk_id,ncntxt,istat,istat_exp
      character*(cfllen) catnam_in,catnam_out
      character*(*) disposition
      character*200 errline

      integer lenstr

      call ccif_setup_context(catnam_in,catnam_out,blk_id,ncntxt, 
     +                istat,disposition)

C--- No category found (but context will be assigned for output files
C    unless DISPOSITION set to 'RO').
      IF (ISTAT.EQ.cat_not_present) THEN
        IF (ISTAT_EXP.NE.cat_not_present) THEN
          write(ERRLINE,*) 
     +     ' Warning in ccp4ccif_setup_context: Category '//
     +     catnam_in(1:lenstr(catnam_in))//' not present in data block.'
          CALL CCPERR(2,ERRLINE)
        ENDIF

C--- Loop found
      ELSEIF (ISTAT.EQ.loop_context) THEN
        IF (ISTAT_EXP.EQ.item_context) THEN
          write(ERRLINE,*) ' Error in ccp4ccif_setup_context: '//
     +    'Tried to set up category '//catnam_in(1:lenstr(catnam_in))//
     +    ' as item context when it is loop context.'
          CALL CCPERR(1,ERRLINE)
        ENDIF

C--- Loop found
      ELSEIF (ISTAT.EQ.item_context) THEN
        IF (ISTAT_EXP.EQ.loop_context) THEN
          write(ERRLINE,*) ' Error in ccp4ccif_setup_context: '//
     +    'Tried to set up category '//catnam_in(1:lenstr(catnam_in))//
     +    ' as item context when it is loop context.'
          CALL CCPERR(1,ERRLINE)
        ENDIF

      ELSE
        write(ERRLINE,*) ' Error in ccp4ccif_setup_context: '//
     +    'Unexpected context type for category '//
     +    catnam_in(1:lenstr(catnam_in))
        CALL CCPERR(1,ERRLINE)
      ENDIF

      ISTAT_EXP = ISTAT

      return
      end

      subroutine ccp4ccif_check_type(itmnam, btype_exp, sline_exp)

CDOC  Check requested item type against dictionary
CDOC
CDOC  Arguments :
CDOC
CDOC  ITMNAM          (I)     CHARACTER       Item name
CDOC
CDOC  BTYPE_EXP       (I)     INTEGER         Expected btype
CDOC
CDOC  SLINE_EXP       (I)     INTEGER         Expected sline
CDOC

      IMPLICIT NONE

      include 'cciflib.fh'

      integer btype, sline, btype_exp, sline_exp
      character*(cfllen) itmnam, ccode, pcode
      character*(200) errlin

      integer lenstr
      external lenstr

c_______Check type of data item (real, integer, etc.)
        call ccif_item_type(itmnam, ccode, pcode, btype, sline)
        if (btype.eq.-1) then
         write(errlin,'(A)') 'Requested item '//itmnam(1:lenstr(itmnam))
     +                             //' not found in dictionary!'
         call ccperr(1,errlin)
        elseif (btype.eq.0) then
         write(errlin,'(A)') 'Type of '//itmnam(1:lenstr(itmnam))
     +                             //' not defined in dictionary!'
         call ccperr(1,errlin)
        elseif (btype.ne.btype_exp) then
         write(errlin,'(A,I1,A,I1,A)') 'Expected btype of '
     +     //itmnam(1:lenstr(itmnam))//' (',btype_exp,
     +     ') disagrees with that in dictionary (',btype,').'
         call ccperr(1,errlin)
        endif

      return
      end

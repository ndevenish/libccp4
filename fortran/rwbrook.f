C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C_BEGIN_RWBROOK
C Title: STANDARD COORDINATE FILES AND THEIR USE (FORTRAN 77 IMPLEMENTATION)
C Author: John W. Campbell
C                         
C CONTENTS
C          
C PART I  - THE STANDARD COORDINATE DATA FILE STRUCTURE
C           1.   INTRODUCTION
C           2.   FORMAT OF THE 'ATOM/HETATM'CARDS
C           3.   FORMAT OF THE 'TER' CARDS
C           4.   FORMAT OF THE 'CRYST1' CARD
C           5.   FORMAT OF THE 'SCALE' CARDS
C           6.   STANDARD RESIDUE NAMES
C           7.   ATOM IDENTIFIERS FOR AMINO ACIDS
C           8.   REFERENCES
C                          
C PART II - SUBROUTINES FOR HANDLING BROOKHAVEN FORMAT COORDINATE FILES
C           1.  INTRODUCTION
C           2.  THE SUBROUTINES
C           3.  EXAMPLE OF READING A FILE
C           4.  EXAMPLE OF READING A FILE AND WRITING A MODIFIED FILE
C           5.  COMMON BLOCKS USED BY THE SUBROUTINES
C           6.  AVAILABILITY OF THE SUBROUTINES
C           7.  DESCRIPTION OF INDIVIDUAL SUBROUTINES:
C    
C  RBROOK     Read a coordinate record
C  RBINIT     Initialisation call
C  RBFROR     generates the various orthogonalising matrices     
C  RBRINV     invert 4*4 matrices for conversion between fractional and orthogonal axe
C  RBFRAC     calc transformation matrices bet orthogonal and fractional coordinates
C  RBRORF     fill or return RF and Ro matrice
C  RBFRO1     duplicate of rbfror with a different call
C  WBROOK     Write a coordinate record.
C  RWBFIN     Complete copy of input to output file
C  CVFRAC     Convert between orthogonal and fractional coordinates
C  RBRCEL     Returns Reciprocal cell dimensions, and reciprocal unit cell  volume
C  RES3TO1    Find 3 character residue name from 1 character code and vice versa 
C  RBRECIP    calculates 4SIN**2/L**2
C
C PART I  -  THE STANDARD COORDINATE DATA FILE STRUCTURE
C                                                       
C 1.   INTRODUCTION 
C                   
C      The standard coordinate data file format adopted for the  CCP  protein
C crystallography program suite is that of the Brookhaven Protein  Data  Bank
C (ref.  1).  The  programs  will  handle  either  complete  files  or  files
C containing only a subset of the types of record which may be present  in  a
C complete file. In particular, the records containing  the  coordinate  data
C (ATOM or HETATM records) are of interest. Their structures and those of the
C TER, CRYST1 and SCALE records, which are also used  by  the  file  handling
C subroutines described in Part II, are outlined below. The Brookhaven format
C defines  a  standard  setting  of  orthogonal  axes  with  respect  to  the
C crystallographic axes and this has been adopted as  the  standard  for  the
C CCP. The standard set of orthogonal axes  XO,  YO  and  ZO  is  defined  as
C follows:
C         
C                                 XO // a
C                                 YO // c* * a
C                                 ZO // c*
C                                          
C Within a Brookhaven format file, however,  coordinates  may  be  held  with
C respect to other sets of axes. When the  files  are  accessed  by  programs
C using fractional coordinates, the CRYST1 or SCALE cards must be present  if
C the coordinates are in the standard setting and the  SCALE  cards  must  be
C present if another setting is used. A  complete  description  of  the  file
C format is available from the Brookhaven Protein Data Bank but some selected
C features, relevant to the handling of the  coordinate  data  are  described
C below. In general terms it may be noted that the format is basically a card
C image format with fixed length 80 byte records.
C                                                
C [An EJD modification to standard Brookhaven is to check whether the
C formfactor number is written as an integer in columns 67-70.
C Keeping the formfactor as an integer saves an appreciable amount of
C time when reading this clumsy file...]
C
C 2.   FORMAT OF THE 'ATOM/HETATM' CARDS
C                                       
C      The format of an 'ATOM' card or 'HETATM' card is as follows:
C
C Cols.  1-4    ATOM (or cols. 1-6 HETATM)
C        7-11   Atom serial number (see note i)
C Cols. 13-14   Chemical symbol (right justified)  )
C       15      Remoteness indicator                 Atom name (see note ii)
C       16      Branch designator                  )
C       17      Alternate location indicator (see note iii)
C       18-20   Residue name (see note iv)
C       21      Reserved                   )
C       22      Chain identifier           )
C                                           Sequence identifier (see note v)
C       23-26   Residue sequence number    )
C       27      Code for inserting residue )
C       31-38   X   )
C       39-46   Y    Orthogonal Angstrom coordinates
C       47-54   Z   )
C       55-60   Occupancy
C       61-66   Temperature factor
C       68-70   Footnote number
C
C Typical format:  (6A1,I5,1X,A4,A1,A3,1X,A1,I4,A1,3X,3F8.3,2F6.2,1X,I3)
C                                                                       
C Notes:    (i)  Residues occur in order  of  their  sequence  numbers  which
C                always increase starting from the N-terminal residue. Within
C                each  residue,  the  atoms  are  ordered  as  indicated   in
C                paragraph 7 below. If the residue sequence is known, certain
C                serial numbers may be omitted to allow for the future inser-
C                tion of any missing atoms. If the sequence is  not  reliably
C                known these serial numbers are simply ordinals.
C                                                               
C          (ii)  The atom names are described in paragraph 7 below.
C                                                                  
C         (iii)  Alternate locations for atoms may be denoted by A, B, C etc.
C                here.
C                     
C          (iv)  The standard residue names are given in paragraph 6 below.
C                                                                          
C           (v)  The sequence identifier is a  composite  field  made  up  as
C                follows:
C                Cols. 21      Reserved for future expansion
C                      22      Chain identifier, e.g. A for Haemoglobin alpha
C                              chain
C                      23-26   Residue sequence number
C                      27      Code for insertions of residues, e.g. 66A, 66B
C                              etc.
C                                  
C 3.   FORMAT OF THE 'TER' CARDS
C                               
C      'TER' cards are used to indicate chain terminations. They  are  placed
C at the appropriate positions within the atom cards. The format of  a  'TER'
C card is as follows:
C                    
C Cols.  1-3    TER
C        7-11   Serial number
C       18-20   Residue name
C       21-27   Sequence identifier (see description of 'ATOM' cards above)
C                                                                          
C Typical format:  (6A1,I5,6X,A3,1X,A1,I4,A1)
C                                            
C 4.   FORMAT OF THE 'CRYST1' CARD
C                                 
C      This card holds the cell parameters and has the following format
C                                                                      
C              Cols.  1-6    CRYST1
C                     7-15   a (Angstroms)
C                    16-24   b (Angstroms)
C                    25-33   c (Angstroms)
C                    34-40   alpha (degrees)
C                    41-47   beta (degrees)
C                    48-54   gamma (degrees)
C                    56-66   Space group symbol (left justified)
C                    67-70   Z
C                             
C Typical Format:  (6A1,3F9.3,3F7.2,1X,11A1,I4)
C                                              
C 5.   FORMAT OF THE 'SCALE' CARDS
C                                 
C      These cards hold the matrix for  transforming  the  stored  orthogonal
C Angstrom coordinates  to  fractional  crystallographic  coordinates.  Three
C cards are required. 'S' is the rotation matrix and 'U' is  the  translation
C matrix. The format of the cards is as follows.
C                                               
C                 Cols.  1-6      SCALE1     SCALE2     SCALE3
C                       11-20      S11        S21        S31
C                       21-30      S12        S22        S32
C                       31-40      S13        S23        S33
C                       46-55      U1         U2         U3
C                                                          
C                  Typical Format:  (6A1,4X,3F10.5,5X,F10.5)
C                                                           
C 6.   STANDARD RESIDUE NAMES
C                            
C      The residue abbreviations for the amino acids conform to the IUPAC-IUB
C rules  (ref.  2).  Non-standard  residues  are  given  a  three   character
C abbreviation chosen by the user. The amino acids  and  their  abbreviations
C are given in the table below.
C                              
C Residue                 Abb.              Residue                    Abb.
C
C Acidic unknown          ACD               Homoserine                    HSE
C Acetyl                  ACE               Hydroxyproline                HYP
C Alanine                 ALA               Hydroxylysine                 HYL
C beta-Alanine            ALB               Isoleucine                    ILE
C Aliphatic unknown       ALI               Leucine                       LEU
C gamma-Aminobutyric acid ABU               Lysine                        LYS
C Arginine                ARG               Methionine                    MET
C Aromatic unknown        ARO               Ornithine                     ORN
C Asparagine              ASN               Phenylalanine                 PHE
C Aspartic acid           ASP               Proline                       PRO
C ASP/ASN ambiguous       ASX               Pyrollidone carboxylic acid   PCA
C Basic unknown           BAS               Sarcosine                     SAR
C Betaine                 BET               Serine                        SER
C Cysteine                CYS               Taurine                       TAU
C Cystine                 CYS               Terminator                    TER
C Formyl                  FOR               Threonine                     THR
C Glutamic acid           GLU               Thyroxine                     THY
C Glutamine               GLN               Tryptophan                    TRP
C GLU/GLN ambiguous       GLX               Tyrosine                      TYR
C Glycine                 GLY               Unknown                       UNK
C Heterogen               HET               Valine                        VAL
C Histidine               HIS               Water                         HOH
C
C 7.   ATOM IDENTIFIERS FOR AMINO ACIDS
C                                      
C      The atom names used follow the IUPAC-IUB rules (ref.  3)  except  that
C the Greek letter remoteness codes are transliterated as follows:-
C                                                                  
C        alpha - A       beta - B       gamma - G       delta - D
C        epsilon - E     zeta - Z       eta - H
C                                              
C Four characters are reserved for the atom names as follows:-
C                                                             
C                   1-2   Chemical symbol - right justified
C                   3     Remoteness indicator (alphabetic)
C                   4     Branch designator (numeric)
C                                                    
C Within each  residue  the  atoms  occur  in  the  order  specified  by  the
C superscripts in the table below. The "extra" oxygen of the carboxy terminal
C amino acid is designated OXT.
C                              
C 8.   REFERENCES
C                 
C (1)  F.C. Bernstein, T.F. Koetzle, G.J.B. Williams, E.F. Meyer,  Jr.,  M.D.
C      Brice, J.R. Rodgers, O. Kennard, T. Shimanouchi and M. Tasumi, J. Mol.
C      Biol., 112 , 535-42 (1977).
C                                  
C (2)  J. Biol. Chem., 241 , 527, 2491 (1966).
C                                              
C (3)  IUPAC-IUB Commission on Biological  Nomenclature.  "Abbreviations  and
C      Symbols for the Description of the Conformation of Polypeptide Chains.
C      Tentative Rules (1969)", J. Biol. Chem., 245 , 6489 (1970).
C                                                                  
C PART II  -  SUBROUTINES FOR HANDLING BROOKHAVEN FORMAT COORDINATE FILES
C                                                                        
C                                                                        
C 1.   INTRODUCTION 
C                   
C      Some subroutines have been written at  the  Daresbury  Laboratory  for
C reading and writing coordinate records  for  Brookhaven  format  coordinate
C files. The Brookhaven format defines a standard setting of orthogonal  axes
C with respect to the crystallographic axes and this is used as the  standard
C within the CCP protein crystallography program suite. The standard  set  of
C orthogonal axes XO, YO, ZO is defined as follows:
C                                                  
C                                XO  //  a
C                                YO  //  c* * a
C                                ZO  //  c*
C                                           
C      Coordinates may be held in orthogonal Angstroms with respect to  other
C axes if required. The subroutine RBROOK, which is used to  read  coordinate
C data, will return the coordinates as stored. If the CRYST1 card is  present
C in the file then the matrices for converting between fractional coordinates
C and orthogonal coordinates in the standard setting will  be  calculated  on
C the first call to RBROOK. This first call to RBROOK stores the matrices for
C converting between orthogonal stored coordinates and fractional coordinates
C if they can be determined. These will be the matrices calculated  from  the
C CRYST1 card calculated for the standard setting  if  no  SCALE  cards  were
C present or will be the matrices read and calculated from the SCALE cards if
C these are present. Thus, when a program  requires  fractional  coordinates,
C the CRYST1 and/or the SCALE cards must be present if the coordinates are in
C the standard setting and the SCALE cards must be present if the coordinates
C are in a non-standard setting.
C                               
C 2.   THE SUBROUTINES
C                     
C      The following subroutines are available for handling Brookhaven format
C coordinate files:
C                  
C    Reading files
C                 
C        . RBINIT    Initialisation call.
C        . RBROOK    Read a coordinate record.
C                                             
C    Writing files
C                 
C        . WBROOK    Write a coordinate record.
C        . RWBFIN    Complete copy of input to output file.
C                                                          
C    Converting coordinates
C                          
C        . CVFRAC    Convert between orthogonal and fractional coordinates
C                                                                         
C      The subroutine RBROOK is used to read coordinates  from  a  Brookhaven
C format coordinate file in a single pass through the  file.  The  subroutine
C RBINIT is called  for  initialisation  prior  to  reading  coordinates  via
C RBROOK.  The  subroutine  RBROOK  also  has  an   option   to   write   the
C non-ATOM/HETATM records to an output file as they are read from  the  input
C file. The subroutine WBROOK  is  used  to  write  the  updated  ATOM/HETATM
C records. The subroutine RWBFIN may be used to copy  all  remaining  records
C from the input file to the output file. The subroutine CVFRAC may  be  used
C to convert between orthogonal and fractional coordinates providing that the
C relevant matrices have been set up by the first call to RBROOK.
C
C 3.   EXAMPLE OF READING A FILE
C                               
C      The example below outlines the way in which the subroutines RBINIT and
C RBROOK are used to read coordinates from Brookhaven format coordinate  file
C (on unit 1)
C
C C PROGRAM TO READ BROOKHAVEN FILE
C C ===============================
C C
C       CHARACTER*4 ATNAM,RESNAM,RESNO
C       CHARACTER*1 CHNNAM
C C
C C INITIALISATION CALL
C C
C       CALL  RBINIT(1)
C C
C C READ COORDINATE RECORDS
C C
C 10    CALL RBROOK(1,ISER,ATNAM,RESNAM,CHNNAM,IRS,RESNO,IS,X,Y,Z,
C      *OCC,B,IZ,0,0,6,0,*10,*100)
C       .
C       .
C       process data
C       .
C       .
C       GO TO 10
C C
C C END OF FILE REACHED
C C
C 100   END
C  
C 4.   EXAMPLE OF READING A FILE AND WRITING A MODIFIED FILE
C                                                           
C      The following example reads in a set of coordinates and processes them
C (e.g. in a refinement). The input file is then re-read and an  output  file
C is created with the updated coordinate data.
C                                             
C C PROGRAM TO READ AND UPDATE A BROOKHAVEN COORDINATE FILE
C C =======================================================
C C
C C SPECIFICATION STATEMENTS
C C
C       CHARACTER*4 ATNAM(1000),RESNAM(1000),RESNO
C       CHARACTER*1 CHNNAM
C       DIMENSION X(1000),Y(1000),Z(1000)
C C
C C READ COORDINATES FROM UNIT 1 AND STORE THEM
C C
C       N=0
C       CALL RBINIT(1)
C 10    N=N+1
C       CALL RBROOK(1,ISER,ATNAM(N),RESNAM(N),CHNNAM,IRESN,RESNO,IS,
C      *X(N),Y(N),Z(N),OCC,B,IZ,0,0,6,0,*10,*100)
C       GO TO 10
C C
C C REFINE COORDINATES
C C
C 100   NMAX=N-1
C       .
C       .
C       refine coordinate values
C       .
C       .
C C
C C WRITE UPDATED COORDINATE FILE (UNIT 2)
C C
C       CALL RBINIT(1)
C       DO 150 N=1, NMAX
C       CALL RBROOK(1,ISER,ATNAM(N),RESNAM(N),CHNNAM,IRES,RESNO,IS,X,Y,Z,
C      *OCC,B,IZ,2,0,6,0,*200,*200)
C       CALL WBROOK(2,ISER,ATNAM(N),RESNAM(N),CHNNAM,IRES,IS,X(N),Y(N),Z(N),
C      *OCC,B,IZ)
C 150   CONTINUE
C C
C C COPY ANY REMAINING RECORDS TO OUTPUT FILE
C C
C 200   CALL RWBFIN(1,2)
C       END
C
C  
C 5.   COMMON BLOCKS USED BY THE SUBROUTINES
C                                            
C       The following common blocks are used to pass information between  the
C subroutines and to hold the contents of the last record read from an  input
C file via RBROOK:
C
C   LOGICAL IFCRYS,IFSCAL,IFEND,MATRIX
C   COMMON /RBRKXX/ IFCRYS,IFSCAL,IFEND,ITYP,MATRIX
C
C   Arguments:    IFCRYS  Flag set to .TRUE. if CRYST1 card read, otherwise
C                           .FALSE.
C                   IFSCAL  Flag set to .TRUE. if SCALE cards read, otherwise
C                           .FALSE.
C                    IFEND  Flag set  to  .TRUE.  if  end  of  file  reached,
C                           otherwise .FALSE.
C                     ITYP  Type of last card read.
C                           =0, No card read.
C                           =1, 'CRYST1'
C                           =2, 'SCALE'
C                           =3, 'TER'
C                           =4, 'ATOM'
C                           =5, 'HETATM'
C                   MATRIX  Flag  set  to  .TRUE.  if   orthogonalising   and
C                           fractionalising matrices set, otherwise .FALSE.
C
C   CHARACTER*1 BROOK
C   COMMON /RBRKYY/ BROOK(72)
C
C   Arguments:     BROOK  Character array  holding  the  last  record  read
C                           (columns 1-72)
C                                         
C   COMMON /RBRKZZ/ CELL(6),RR(3,3,5)
C                                    
C   Arguments:      CELL  Array holding cell dimensions  read  from  CRYST1
C                           card if read (check IFCRYS)
C                       RR  Array  holding  some   common   orthogonalisation
C                           matrices calculated if  CRYST1  cards  was  read.
C                           (See description of NCODE in common ORTHOG)
C                                                                      
C   COMMON /ORTHOG/ RO(4,4),RF(4,4),NCODE,IBRKFL
C                                               
C   Arguments:        RO  Orthogonalising matrix (only  set  if  CRYST1  or
C                           SCALE cards read (check the flag MATRIX)
C                       RF  Fractionalising matrix (only  set  if  CRYST1  or
C                           SCALE cards read (check the flag MATRIX)
C                    NCODE  Flag  indicating  setting  found,   =0   if   not
C                           recognised.
C                           =1, a // XO,  c* // ZO (Standard Brookhaven)
C                           =2, b // XO,  a* // ZO
C                           =3, c // XO,  b* // ZO
C                           =4, Hexagonal a + b // XO, c* // ZO
C                           =5, a* // XO, c // ZO (Rollett)
C                   IBRKFL  Flag = 0, Brookhaven file, non-zero reserved  for
C                           some other type.
C
C 7. Subroutines in RWBROOK:
C
C_END_RWBROOK
C_BEGIN_RBROOK
C
      SUBROUTINE RBROOK(IUN,ISER,ATNAM,RESNAM,CHNNAM,IRESN,RESNO,IS,
     *X,Y,Z,OCC,B,IZ,IOUT,MSG1,MSG2,ITER,*,*)
C     ==============================================================
C
C
C      The subroutine RBROOK is used to read coordinates  from  a  Brookhaven
C format coordinate file. The subroutine RBINIT must be called prior to read-
C ing or re-reading a  file  via  RBROOK.  If  required  the  non-ATOM/HETATM
C records may be written to an output file as they are read. The  first  call
C sets up the matrices from the  CRYST1  or  SCALE  cards,  if  present,  for
C converting between stored orthogonal and fractional coordinates.
C 
C Call:  CALL RBROOK(IUN,ISER,ATNAM,RESNAM,CHNNAM,IRESN,RESNO,IS,X,Y,Z,OCC,B,
C       *IZ,IOUT,MSG1,MSG2,ITER,*label1,*label2)
C
C Parameters
C
C         IUN (I) Unit no. of the input coordinate file
C        ISER (O) Atom serial number
C       ATNAM (O) Atom name        (character*4 left justified)
C      RESNAM (O) Residue name     (character*4)
C      CHNNAM (O) Chain name       (character*1)
C       IRESN (O) Residue number as an integer
C       RESNO (O) Residue number   (character*4 or character*5)
C                 If character*5 then the 5th character will be the
C                 insertion code.
C          IS (O) Reserved flag (ejd). not part of brookhaven defn.
C           X (O) Coordinates (orthogonal angstrom coordinates as
C           Y (O)     "        stored)
C           Z (O)     "
C         OCC (O) Occupancy
C           B (O) Temperature factor
C          IZ (O) Atomic number (returned as 7 from ambiguous atoms)
C        IOUT (I) Unit number to which non-atom/hetatm records are to
C                 be be written (may be 0 if reading only)
C        MSG1 (I) unit number for flagging ambiguous/unknown atom types
C                 (may be 0 if messages are not required)
C        MSG2 (I) unit no. for listing matrix (if calculated)
C                 (may be 0 if no listing required)
C        ITER (I) FLAG =1, return if 'ter' card found (via return 1)
C                      =0, do not return when 'ter' card found
C
C  RETURN 1   RETURN ON 'TER' CARD FOUND (ONLY IF ITER=1)
C  RETURN 2   RETURN ON END OF FILE FOUND
C
C  COMMON BLOCKS
C
C  COMMON /RBRKXX/IFCRYS,IFSCAL,IFEND,ITYP,MATRIX
C
C      IFCRYS   .TRUE. IF 'CRYST1' CARD READ,  OTHERWISE .FALSE.
C      IFSCAL   .TRUE. IF 'SCALE' CARDS READ, OTHERWISE .FALSE.
C      IFEND    .TRUE. IF END OF FILE REACHED, OTHERWISE .FALSE.
C       ITYP    TYPE OF LAST CARD READ =1, 'CRYST1'
C                                      =2, 'SCALE'
C                                      =3, 'TER'
C                                      =4, 'ATOM'
C                                      =5, 'HETATM'
C     MATRIX    .TRUE. IF FRACT/ORTHOG MATRICES CALCULATED
C               .FALSE. IF NOT
C
C  COMMON /RBRKYY/BROOK(80)
C
C      BROOK    CHARACTER*1 ARRAY HOLDING LAST RECORD READ (COLS 1-72)
C
c      COMMON/RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
C
C       CELL    CELL DIMENSIONS FROM 'CRYST1' CARD IF READ
C               (CHECK IFCRYS)
C         RR    STANDARD ORTHOGONALISING MATRICES CALCULATED IF THE
C               'CRYST1' CARD WAS READ (CHECK IFCRYS)
C
C  COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE,IBRKFL
C
C        RO    ORTHOGONALISING MATRIX (ONLY SET IF 'CRYST1' OR 'SCALE'
C              CARDS PRESENT - CHECK 'MATRIX' FLAG)
C        RF    FRACTIONALISING MATRIX (ONLY SET IF 'CRYST1' OR 'SCALE'
C              CARDS PRESENT - CHECK 'MATRIX' FLAG)
C     NCODE    FLAG INDICATING SETTING FOUND, 0 IF NOT ONE THAT WAS
C              RECOGNISED
C    IBRKFL    =0, BROOKHAVEN FILE, NON-ZERO FOR SOME OTHER TYPE
C
C_END_RBROOK
C
C
      LOGICAL IFCRYS,IFSCAL,IFEND,IFTER,MATRIX
      CHARACTER*72 BROOKA
      CHARACTER*40 ORTH(5)
      CHARACTER*4 ATNAM,RESNAM,ITYPE(5),IRTYPE
      CHARACTER*(*) RESNO
      CHARACTER*2 IATM(100),IEC(3),IE,IAA,IAT,IHATM(10)
      CHARACTER*1 ISP,CHNNAM
      DIMENSION P(4,4),celchk(6)
      COMMON /RBRKXX/IFCRYS,IFSCAL,IFEND,ITYP,MATRIX
      CHARACTER*1 BROOK
      COMMON /RBRKYY/BROOK(80)
      COMMON/RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE,IBRKFL
      EQUIVALENCE (IRTYPE,BROOK(1)),(IE,BROOK(5)),(BROOKA,BROOK(1))
      SAVE /RBRKXX/,/RBRKYY/,/RBRKZZ/,/ORTHOG/
      DATA IATM/' H','HE','LI','BE',' B',' C',' N',' O',' F','NE',
     *          'NA','MG','AL','SI',' P',' S','CL','AR',' K','CA',
     *          'SC','TI',' V','CR','MN','FE','CO','NI','CU','ZN',
     *          'GA','GE','AS','SE','BR','KR','RB','SR',' Y','ZR',
     *          'NB','MO','TC','RU','RH','PD','AG','CD','IN','SN',
     *          'SB','TE',' I','XE','CS','BA','LA','CE','PR','ND',
     *          'PM','SM','EU','GD','TB','DY','HO','ER','TM','YB',
     *          'LU','HF','TA',' W','RE','OS','IR','PT','AU','HG',
     *          'TL','PB','BI','PO','AT','RN','FR','RA','AC','TH',
     *          'PA',' U','NP','PU','AM','CM','BK','CF','ES','FM'/
      DATA IHATM/'0H','1H','2H','3H','4H','5H','6H','7H','8H','9H'/
      DATA ITYPE/'CRYS','SCAL','TER ','ATOM','HETA'/
      DATA IEC/'E1','E2','E3'/
      DATA IAA/' A'/,ISP/' '/
      DATA ORTH/'A // XO, C* // ZO (STANDARD BROOKHAVEN)',
     *          'B // XO, A* // ZO',
     *          'C // XO, B* // ZO',
     *          'HEX A+B // XO, C* // ZO',
     *          'A* // XO, C // ZO (ROLLETT)'/
C
C
      IFTER=.FALSE.
      IS=0
C
C---- Read next record and branch on record type
C
10    READ(IUN,1001,END=600)BROOK
C
C---- Cell card found - calculate standard orthogonalising matrix
C     Check if you already have a cell which is inconsistent with 
C     this one
C
      IF (IRTYPE.EQ.'CRYS') THEN
        ITYP=1
        IFCRYS=.TRUE.
        ICHK=1
        CELCHK(1)=CELL(1)
        CELCHK(2)=CELL(2)
        CELCHK(3)=CELL(3)
        CELCHK(4)=CELL(4)
        CELCHK(5)=CELL(5)
        CELCHK(6)=CELL(6)
        IF(CELCHK(1).EQ.0.0 .OR. CELCHK(2).EQ.0.0 .OR. CELCHK(3).EQ.0.)
     +    ICHK=0
C
        READ(BROOKA,1003)CELL
C
        IF (ICHK.EQ.1) THEN
          DO 111 I=1,6
            CELDEL = ABS(CELCHK(I)-CELL(I))/CELCHK(I)
            IF(CELDEL.GT.0.01)THEN
              WRITE(6,987)CELCHK,CELL
987          FORMAT(' Inconsistency in Cell Dimensions',2(/,3X,6F10.5))
            ENDIF
111       CONTINUE
        ENDIF
C
        CALL RBFROR
        IF(NCODE.EQ.0)NCODE=1
C
        DO 1101 I=1,3
          DO 110 J=1,3
            RO(I,J)=RR(I,J,NCODE)
110       CONTINUE
1101    CONTINUE
C
        RO(4,4)=1.0
        CALL RBRINV(RO,RF)
        MATRIX=.TRUE.
        IF(MSG2.LE.0)GO TO 500
C
        WRITE(MSG2,1009)(RF(1,J),J=1,4),(RO(1,K),K=1,4),
     +                  (RF(2,J),J=1,4),(RO(2,K),K=1,4),
     +                  (RF(3,J),J=1,4),(RO(3,K),K=1,4),
     +                  (RF(4,J),J=1,4),(RO(4,K),K=1,4)
C
C---- Scale cards - extract and calculate rotation and trans matrices
C
      ELSE IF (IRTYPE.EQ.'SCAL') THEN
        ITYP=2
        MATRIX=.FALSE.
        DO 210 I=1,3
          IF(IE.NE.IEC(I))GO TO 210
          READ(BROOKA,1004)P(I,1),P(I,2),P(I,3),P(I,4)
          GO TO 220
210     CONTINUE
C
C
        GO TO 500
220     IF(I.NE.3)GO TO 500
        MATRIX=.TRUE.
        DO 2351 I=1,3
          DO 235 J=1,4
            RF(I,J)=P(I,J)
235       CONTINUE
2351    CONTINUE
C
C---- Find orthogonalisation type
C
        CALL RBRINV(RF,RO)
        VOLCHK = RO(1,1)*(RO(2,2)*RO(3,3) - RO(2,3)*RO(3,2))
     +         + RO(1,2)*(RO(2,3)*RO(3,1) - RO(2,1)*RO(3,3))
     +         + RO(1,3)*(RO(2,1)*RO(3,2) - RO(2,2)*RO(3,1))
C
        ERROR = ABS(VOLCHK - VOL) /VOL
         IF(error.gt.0.02) then
        WRITE (6,'(//,A,F15.4)')
     +     ' Unit cell volume generated from SCALEi cards', VOLCHK
        WRITE (6,'(//,A,F15.4)')
     +     ' Percentage error is                        ', ERROR
         END IF
C
        IF (ERROR.GT.0.1) call ccperr(1,
     +  ' stop in rwbrook.for - disagreement between cell and PDB file')
C
        DO 250 IORTH=1,6
          DO 245 I=1,3
            DO 240 J=1,3
              IF(ABS(RO(I,J)-RR(I,J,IORTH)).GT.0.001)GO TO 250
240         CONTINUE
245       CONTINUE
          GO TO 255
250     CONTINUE
        IORTH=0
255     NCODE=IORTH
C   Correct inaccuracy of SCALEi input due to FORMAT, replace RF,RO 
C    with RR(...,NCODE) if possible.
        IF(NCODE.GT.0) THEN
          DO 256 I = 1,3
          DO 256 J = 1,3
           RO(I,J) = RR(I,J,NCODE)
256       CONTINUE
        CALL RBRINV(RO,RF)
        END IF

        IF(MSG2.LE.0)GO TO 500
C
        WRITE(MSG2,1002)(RF(1,J),J=1,4),(RO(1,K),K=1,4),
     +                  (RF(2,J),J=1,4),(RO(2,K),K=1,4),
     +                  (RF(3,J),J=1,4),(RO(3,K),K=1,4),
     +                  (RF(4,J),J=1,4),(RO(4,K),K=1,4)
        IF(IORTH.GT.0)WRITE(MSG2,1007)ORTH(IORTH)
        IF(P(1,4).NE.0.OR.P(2,4).NE.0.OR.P(3,4).NE.0)WRITE(MSG2,1008)
        GO TO 500
C
C---- Atom/hetatm card processing
C
      ELSE IF (IRTYPE.EQ.'ATOM' .OR. IRTYPE.EQ.'HETA' .OR.
     +       IRTYPE.EQ.'TER ') THEN
        IF (IRTYPE.EQ.'TER ') THEN
C
C---- 'ter' card found
C
          ITYP=3
          IFTER=.TRUE.
          GO TO 450
        ENDIF
        IF (IRTYPE.EQ.'ATOM') ITYP=4
        IF (IRTYPE.EQ.'HETA') ITYP=5
400     READ(BROOKA,1005)IS,X,Y,Z,OCC,B,I
        IF(BROOK(13).EQ.ISP)GO TO 410
        ATNAM=BROOK(13)//BROOK(14)//BROOK(15)//BROOK(16)
        GO TO 450
410     ATNAM=BROOK(14)//BROOK(15)//BROOK(16)//BROOK(17)
450     READ(BROOKA,1006)ISER,IRESN
        RESNAM=BROOK(18)//BROOK(19)//BROOK(20)//ISP
        RESNO=BROOK(23)//BROOK(24)//BROOK(25)//BROOK(26)
        IF(LEN(RESNO).GT.4)RESNO(5:5)=BROOK(27)
        CHNNAM=BROOK(22)
        IF(IFTER)GO TO 500
        IF(I.GT.0)GO TO 480
        IAT=BROOK(13)//BROOK(14)
C
        DO 452 I=6,8
          IF(IAT.EQ.IATM(I))GO TO 480
452     CONTINUE
C
        I=1
C
        DO 454 J=1,10
          IF(IAT.EQ.IHATM(J))GO TO 480
454     CONTINUE
C
        DO 456 I=1,100
          IF(IAT.EQ.IATM(I))GO TO 480
456     CONTINUE
C
        I=0
        IF(IAT.EQ.IAA)I=7
        IF(MSG1.GT.0.AND.I.EQ.0)WRITE(MSG1,2001)ATNAM,RESNAM,RESNO
        IF(MSG1.GT.0.AND.I.EQ.7)WRITE(MSG1,2002)ATNAM,RESNAM,RESNO
480     IZ=I
C
C---- Reset Formfactor for W to that of O - in proteins it is prob H2O
C
        IF(IZ.EQ.74) IZ = 8
        RETURN
C
C---- Write record to output file if required (unless atom/hetatm)
C
      ELSE
501     IF(IOUT.GT.0)WRITE(IOUT,1001)BROOK
        IF(ITER.EQ.1.AND.IFTER)RETURN 1
        IFTER=.FALSE.
      ENDIF
      GO TO 10
500     IF(IOUT.GT.0)WRITE(IOUT,1001)BROOK
        IF(ITER.EQ.1.AND.IFTER)RETURN 1
        IFTER=.FALSE.
      GO TO 10
C
C---- End of file found
C
600   IFEND=.TRUE.
      RETURN 2
C
C---- Format statements
C
1001  FORMAT(80A1)
1002  FORMAT(/,' MATRICES DERIVED FROM SCALE CARDS IN COORDINATE FILE',
     *//,'            RF                                   RO',/,
     *4(/,1X,4F8.3,5X,4F8.3),//)
1003  FORMAT(6X,3F9.3,3F7.2)
1004  FORMAT(10X,3F10.6,5X,F10.6)
1005  FORMAT(27X,I2,1X,3F8.3,2F6.2,I4)
1006  FORMAT(6X,I5,11X,I4)
1007  FORMAT(/,' ORTHOGONALISATION CODE: ',A40,/)
1008  FORMAT(/,' TRANSLATIONS ALSO SPECIFIED',/)
1009  FORMAT(/,' MATRICES DERIVED FROM CRYST1 CARD IN COORDINATE FILE',
     *//,'            RF                                  RO',/,
     *4(/,1X,4F8.3,5X,4F8.3),//)
2001  FORMAT(' **UNKNOWN ATOMIC FORMFACTOR ',A4,' IN ',A4,1X,A4,'**')
2002  FORMAT(' *AMBIGUOUS ATOMIC FORMFACTOR ',A4,' IN ',A4,1X,A4,'*')
      END
C
C
C
      SUBROUTINE RBINIT(IUN)
C     ======================
C
C_BEGIN_RBINIT
C
C   call  RBINIT(IUN)
C
C
C This subroutine is used to rewind a coordinate file and to
C initialise values in the common /rbrkxx/ before calling RBROOK
C
C
C PARAMETERS
C
C      
C         IUN (I) (INTEGER)  UNIT NO. OF THE COORDINATE FILE
C
C_END_RBINIT
C
C
      LOGICAL IFCRYS,IFSCAL,IFEND,MATRIX
      COMMON /RBRKXX/IFCRYS,IFSCAL,IFEND,ITYP,MATRIX
      CHARACTER*1 BROOK
      COMMON /RBRKYY/BROOK(80)
      COMMON /RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE,IBRKFL
      SAVE /RBRKXX/,/RBRKYY/,/RBRKZZ/,/ORTHOG/
C
C---- Rewind file, set flags and default matrix
C
      REWIND IUN
      IFCRYS=.FALSE.
      IFSCAL=.FALSE.
      IFEND=.FALSE.
      MATRIX=.FALSE.
      NCODE=0
      ITYP=0
      IBRKFL=0
C
C
      DO 20 I=1,3
      DO 21 J=I+1,4
      RO(I,J)=0.0
      RO(J,I)=0.0
      RF(I,J)=0.0
      RF(J,I)=0.0
21    CONTINUE
      RO(I,I)=1.0
      RF(I,I)=1.0
20    CONTINUE
C
C
      RO(4,4)=1.0
      RF(4,4)=1.0
      RETURN
      END
C
C
C
      SUBROUTINE RBFRAC(A,B,C,AL,BE,GA,MSG)
C     =====================================
C
C_BEGIN_RBFRAC
C
C      SUBROUTINE RBFRAC(A,B,C,AL,BE,GA,MSG)
C
C
C This subroutine is used to calculate the default transformation
C matrices between orthogonal angstrom and fractional coordinates
C
C
C PARAMETERS
C
C    A,B,C,AL,BE,GA (I) (REAL)     CELL PARAMETERS IN ANGSTROMS AND DEGREES
C               MSG (I) (INTEGER)  UNIT NO. FOR PRINTING MESSAGE (0 IF NOT REQUIRED)
C
C_END_RBFRAC
C
      LOGICAL IFCRYS,IFSCAL,IFEND,MATRIX
      COMMON /RBRKXX/IFCRYS,IFSCAL,IFEND,ITYP,MATRIX
      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE,IBRKFL
      COMMON/RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
      SAVE /RBRKXX/,/ORTHOG/,/RBRKZZ/
C
C---- Calculate matrices
C
      DO 10 I=1,4
      DO 10 J=1,4
      RO(I,J)=0
      RF(I,J)=0
10    CONTINUE
C
C
      RO(4,4)=1.0
      RF(4,4)=1.0
      CELL(1)=A
      CELL(2)=B
      CELL(3)=C
      CELL(4)=AL
      CELL(5)=BE
      CELL(6)=GA
      CALL RBFROR
C
C
      DO 20 I=1,3
      DO 20 J=1,3
      RO(I,J)=RR(I,J,1)
20    CONTINUE
C
C
      CALL RBRINV(RO,RF)
      MATRIX=.TRUE.
      IF(MSG.GT.0)WRITE(MSG,1001)
      RETURN
C
C---- Format statements
C
1001  FORMAT(/,' STANDARD BROOKHAVEN COORDINATE SETTING WILL BE ASSUMED'
     * ,/, ' IF NO SCALE CARDS PRESENT  IN  INPUT  COORDINATE  FILE',/)
      END
C
C
C
      SUBROUTINE RBRORF(ROO,RFF)
C     =======================
C
C_BEGIN_RBRORF
C
C      SUBROUTINE RBRORF(ROO,RFF)
C
C     Subroutine to  fill or return RF (fractionalising) and Ro
C     (orthogonalising) matrices. This subroutine is called by RBROOK.
C
C PARAMETERS
C
C          ROO (I) (REAL(4,4))  4*4 MATRIX TO BE INVERTED
C          RFF (O) (REAL(4,4))  INVERSE MATRIX
C
C common blocks
C
C      COMMON /RBRKXX/IFCRYS,IFSCAL,IFEND,ITYP,MATRIX
C      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE,IBRKFL
C
C_END_RBRORF

      LOGICAL IFCRYS,IFSCAL,IFEND,MATRIX
      COMMON /RBRKXX/IFCRYS,IFSCAL,IFEND,ITYP,MATRIX
      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE,IBRKFL
      SAVE /ORTHOG/, /RBRKXX/
C
C
      DIMENSION ROO(4,4),RFF(4,4)
C
C---- Get cofactors of 'a' in array 'c'
C
      IF(ROO(1,1) .LE.0.0000000001) THEN 
        DO 40 II=1,4
        DO 30 JJ=1,4
        RFF(II,JJ)=RF(II,JJ)
        ROO(II,JJ)=RO(II,JJ)
30      CONTINUE
40      CONTINUE
        RETURN
      END IF
C  FILL...
      IF(ROO(1,1) .GT.0.0000000001) THEN 
        DO 50 II=1,4
        DO 60 JJ=1,4
        RF(II,JJ)=RFF(II,JJ)
        RO(II,JJ)=ROO(II,JJ)
60      CONTINUE
50      CONTINUE
        MATRIX = .TRUE.
        RETURN
      END IF
      END
C
C
C
      SUBROUTINE RBRINV(A,AI)
C     =======================
C
C_BEGIN_RBRINV
C
C      SUBROUTINE RBRINV(A,AI)
C
C
C Subroutine to invert 4*4 matrices for conversion between
C fractional and orthogonal axes. This subroutine is called from 
C RBROOK.
C
C
C PARAMETERS
C
C           A (I) (REAL(4,4))  4*4 MATRIX TO BE INVERTED
C          AI (O) (REAL(4,4))  INVERSE MATRIX
C
C_END_RBRINV
C
      DIMENSION A(4,4),AI(4,4),C(4,4),X(3,3)
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
      RETURN
      END
C
C
C
      SUBROUTINE RBFROR
C     =================
C
C_BEGIN_RBFROR
C
C      SUBROUTINE RBFROR
C
C THIS SUBROUTINE CALCULATES MATRICES FOR STANDARD ORTHOGONALISATIONS
c   and cell volume
C
C  this generates the various orthogonalising matrices
C     ' NCODE =1 -  ORTHOG AXES ARE DEFINED TO HAVE'
C                    A PARALLEL TO XO   CSTAR PARALLEL TO ZO'
C     ' NCODE =2 -  ORTHOG AXES ARE DEFINED TO HAVE'
C     '               B PARALLEL TO XO   ASTAR PARALLEL TO ZO'
C     ' NCODE =3 -  ORTHOG AXES ARE DEFINED TO HAVE'
C     '               C PARALLEL TO XO   BSTAR PARALLEL TO ZO'
C     ' NCODE =4 -  ORTHOG AXES ARE DEFINED TO HAVE'
C     '         HEX A+B PARALLEL TO XO   CSTAR PARALLEL TO ZO'
C     ' NCODE =5 -  ORTHOG AXES ARE DEFINED TO HAVE'
C     '           ASTAR PARALLEL TO XO       C PARALLEL TO ZO'
C     ' NCODE =6 -  ORTHOG AXES ARE DEFINED TO HAVE'
C                    A  PARALLEL TO XO   BSTAR PARALLEL TO YO'
C
C   SET UP MATRICES TO ORTHOGONALISE H K L AND X Y Z FOR THIS CELL.
C
C Common Blocks
C
C      COMMON/RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
C      COMMON /RBREC/AC(6)
C
C_END_RBFROR
C
      DIMENSION DD(6)
      COMMON/RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
      COMMON /RBREC/AC(6)
      SAVE /RBRKZZ/, /RBREC/
C
C---- Initialisations
C
      CONV=3.14159/180.
      FCT=8.*3.14159*3.14159
      ALPH=CONV*CELL(4)
      BET=CONV*CELL(5)
      GAMM=CONV*CELL(6)
      SUM=0.5*(ALPH+BET+GAMM)
      V=SQRT(SIN(SUM-ALPH)*SIN(SUM-BET)*SIN(SUM-GAMM)*SIN(SUM))
      VOL=2.0*CELL(1)*CELL(2)*CELL(3)*V
      SINA=SIN(ALPH)
      COSA=COS(ALPH)
      SINB=SIN(BET)
      COSB=COS(BET)
      SING=SIN(GAMM)
      COSG=COS(GAMM)
      COSBS=(COSA*COSG-COSB)/(SINA*SING)
      SINBS=SQRT(1.-COSBS*COSBS)
      COSAS=(COSG*COSB-COSA)/(SINB*SING)
      SINAS=SQRT(1.-COSAS*COSAS)
      COSGS=(COSA*COSB-COSG)/(SINA*SINB)
      SINGS=SQRT(1.-COSGS*COSGS)
      A=CELL(1)
      B=CELL(2)
      C=CELL(3)
      AS=B*C*SINA/VOL
      BS=C*A*SINB/VOL
      CS=A*B*SING/VOL
      ALPHAS=ATAN2(SINAS,COSAS)/CONV
      BETAS=ATAN2(SINBS,COSBS)/CONV
      GAMMAS=ATAN2(SINGS,COSGS)/CONV
      CELLAS(1)=AS
      CELLAS(2)=BS
      CELLAS(3)=CS
      CELLAS(4)=ALPHAS
      CELLAS(5)=BETAS
      CELLAS(6)=GAMMAS
C
C       WRITE (6,'(//,A,F15.4)')
C     1 '  Unit cell volume from input cell dimensions ', VOL
C
C---- Set useful things for calculating dstar
C
      AC(1)=AS*AS
      AC(2)=Bs*BS
      AC(3)=Cs*CS
      AC(4)=2.*Bs*Cs*COSAS
      AC(5)=2.*Cs*As*COSBS
      AC(6)=2.*As*Bs*COSGS
C
C---- Zero matrices
C
      DO 5 N=1,6
      DO 5 I=1,3
      DO 5 J=1,3
      RR(I,J,N)=0.0
5     CONTINUE
C
C---- Calculate matrices
C
C   XO along a  Zo along c*
C
10    NCODE=1
      RR(1,1,NCODE)=A
      RR(1,2,NCODE)=B*COSG
      RR(1,3,NCODE)=C*COSB
      RR(2,2,NCODE)=B*SING
      RR(2,3,NCODE)=-C*SINB*COSAS
      RR(3,3,NCODE)=C*SINB*SINAS
C
C---- XO along b  Zo along a*
C
20    NCODE=2
      RR(3,1,NCODE)=A*SING*SINBS
      RR(1,1,NCODE)=A*COSG
      RR(1,2,NCODE)=B
      RR(1,3,NCODE)=C*COSA
      RR(2,1,NCODE)=-A*SING*COSBS
      RR(2,3,NCODE)=C*SINA
C
C---- XO along c  Zo along b*
C
30    NCODE=3
      RR(1,1,NCODE)=A*COSB
      RR(1,2,NCODE)=B*COSA
      RR(1,3,NCODE)=C
      RR(2,1,NCODE)=A*SINB
      RR(2,2,NCODE)=-B*SINA*COSGS
      RR(3,2,NCODE)=B*SINA*SINGS
C
C---- trigonal only - XO along a+b  YO along a-b  Zo along c*
C
40    NCODE=4
      RR(3,3,NCODE)=C
      RR(1,1,NCODE)=A/2.
      RR(1,2,NCODE)=A/2.
      RR(2,1,NCODE)=-A*SING
      RR(2,2,NCODE)=A*SING
C
C---- XO along a*   ZO along c
C
50    NCODE=5
      RR(1,1,NCODE)=A*SINB*SINGS
      RR(3,1,NCODE)=A*COSB
      RR(3,2,NCODE)=B*COSA
      RR(3,3,NCODE)=C
      RR(2,1,NCODE)=-A*SINB*COSGS
      RR(2,2,NCODE)=B*SINA
C
C---- Grr*! to  Gerard Bricogne - his setting for P1 in SKEW.
C   XO along a  Yo along b*
C
60    NCODE=6
      RR(1,1,NCODE)=A
      RR(1,2,NCODE)=B*COSG
      RR(1,3,NCODE)=C*COSB
      RR(2,2,NCODE)=B*SING*SINAS
      RR(3,2,NCODE)=-B*SING*COSAS
      RR(3,3,NCODE)=C*SINB
C
C
      RETURN
      END
C
C
C     ===============================
      SUBROUTINE RBFRO1(CEL,VOLL,RRR)
C     ===============================
C
C_BEGIN_RBFRO1
C
C      SUBROUTINE RBFRO1(CEL,VOLL,RRR)
C
C---- This subroutine is a duplicate of rbfror with a different call.
C
C PARAMETERS
C
C   CEL  (I) (REAL(6))     Cell dimensions
C   VOLL (O) (REAL)        Cell volume
C   RRR  (O) (REAL(3,3,6)) Standard orthogonisational matrices
C
C_END_RBFRO1
C
C     .. Scalar Arguments ..
      REAL VOLL
C     ..
C     .. Array Arguments ..
      REAL CEL(6),RRR(3,3,6)
C     ..
C     .. Scalars in Common ..
      REAL VOL
C     ..
C     .. Arrays in Common ..
      REAL AC,CELL,CELLAS,RR
C     ..
C     .. Local Scalars ..
      REAL A,ALPH,ALPHAS,AS,B,BET,BETAS,BS,C,CONV,COSA,COSAS,COSB,COSBS,
     +     COSG,COSGS,CS,FCT,GAMM,GAMMAS,SINA,SINAS,SINB,SINBS,SING,
     +     SINGS,SUM,V
      INTEGER I,J,K,N,NCODE
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN2,COS,SIN,SQRT
C     ..
C     .. Common blocks ..
      COMMON /RBREC/AC(6)
      COMMON /RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
C     ..
C     .. Save statement ..
      SAVE /RBRKZZ/, /RBREC/
C     ..
C
C---- Initialisations
C
      CELDEL = 0.0
      IF (CEL(1).GT.0.0) THEN
        IF (CELL(1).GT.0.0) THEN
          DO 101 I = 1,6
            CELDEL = ABS(CEL(I)-CELL(I))/CEL(I)
            IF (CELDEL.GT.0.01) WRITE (6,9876) CEL,CELL
9876        FORMAT(' Inconsistency in Cell Dimensions',2(/,3X,6F10.5))
 101      CONTINUE
        ENDIF
        DO 10 I = 1,6
          CELL(I) = CEL(I)
   10   CONTINUE
      ENDIF
      IF (CELL(1).EQ.0.0) call ccperr(1,
     +  ' **** No Cell Input ?? **** from rwbrook.for')
C
C
CC      WRITE (6,FMT=6000) CELL
      CONV = 3.14159/180.0
      FCT = 8.0*3.14159*3.14159
      ALPH = CELL(4)*CONV
      BET = CELL(5)*CONV
      GAMM = CELL(6)*CONV
      SUM = (ALPH+BET+GAMM)*0.5
      V = SQRT(SIN(SUM-ALPH)*SIN(SUM-BET)*SIN(SUM-GAMM)*SIN(SUM))
      VOL = CELL(1)*2.0*CELL(2)*CELL(3)*V
      SINA = SIN(ALPH)
      COSA = COS(ALPH)
      SINB = SIN(BET)
      COSB = COS(BET)
      SING = SIN(GAMM)
      COSG = COS(GAMM)
      COSBS = (COSA*COSG-COSB)/ (SINA*SING)
      SINBS = SQRT(1.0-COSBS*COSBS)
      COSAS = (COSG*COSB-COSA)/ (SINB*SING)
      SINAS = SQRT(1.0-COSAS*COSAS)
      COSGS = (COSA*COSB-COSG)/ (SINA*SINB)
      SINGS = SQRT(1.0-COSGS*COSGS)
      A = CELL(1)
      B = CELL(2)
      C = CELL(3)
      AS = B*C*SINA/VOL
      BS = C*A*SINB/VOL
      CS = A*B*SING/VOL
      ALPHAS = ATAN2(SINAS,COSAS)/CONV
      BETAS = ATAN2(SINBS,COSBS)/CONV
      GAMMAS = ATAN2(SINGS,COSGS)/CONV
      CELLAS(1) = AS
      CELLAS(2) = BS
      CELLAS(3) = CS
      CELLAS(4) = ALPHAS
      CELLAS(5) = BETAS
      CELLAS(6) = GAMMAS
C
C---- Set useful things for calculating dstar
C
      AC(1) = AS*AS
      AC(2) = BS*BS
      AC(3) = CS*CS
      AC(4) = 2.0*BS*CS*COSAS
      AC(5) = 2.0*CS*AS*COSBS
      AC(6) = 2.0*AS*BS*COSGS
C
C---- Zero matrices
C
      DO 40 N = 1,5
        DO 30 I = 1,3
          DO 20 J = 1,3
            RR(I,J,N) = 0.0
   20     CONTINUE
   30   CONTINUE
   40 CONTINUE
C
C---- Calculate matrices
C
C---- XO along a  Zo along c*
C
      NCODE = 1
      RR(1,1,NCODE) = A
      RR(1,2,NCODE) = B*COSG
      RR(1,3,NCODE) = C*COSB
      RR(2,2,NCODE) = B*SING
      RR(2,3,NCODE) = -C*SINB*COSAS
      RR(3,3,NCODE) = C*SINB*SINAS
C
C---- XO along b  Zo along a*
C
      NCODE = 2
      RR(3,1,NCODE) = A*SING*SINBS
      RR(1,1,NCODE) = A*COSG
      RR(1,2,NCODE) = B
      RR(1,3,NCODE) = C*COSA
      RR(2,1,NCODE) = -A*SING*COSBS
      RR(2,3,NCODE) = C*SINA
C
C---- XO along c  Zo along b*
C
      NCODE = 3
      RR(2,1,NCODE) = A*SINB
      RR(2,2,NCODE) = -B*SINA*COSGS
      RR(3,2,NCODE) = B*SINA*SINGS
      RR(1,1,NCODE) = A*COSB
      RR(1,2,NCODE) = B*COSA
      RR(1,3,NCODE) = C
C
C---- trigonal only - XO along a+b  YO alon a-b  Zo along c*
C
      NCODE = 4
      RR(3,3,NCODE) = C
      RR(1,1,NCODE) = A/2.0
      RR(1,2,NCODE) = A/2.0
      RR(2,1,NCODE) = -A*SING
      RR(2,2,NCODE) = A*SING
C
C---- XO along a*   ZO along c
C
      NCODE = 5
      RR(1,1,NCODE) = A*SINB*SINGS
      RR(3,1,NCODE) = A*COSB
      RR(3,2,NCODE) = B*COSA
      RR(3,3,NCODE) = C
      RR(2,1,NCODE) = -A*SINB*COSGS
      RR(2,2,NCODE) = B*SINA
C
C---- Grr*! to  Gerard Bricogne - his setting for P1 in SKEW.
C     XO along a  Yo along b*
C
      NCODE = 6
      RR(1,1,NCODE) = A
      RR(1,2,NCODE) = B*COSG
      RR(1,3,NCODE) = C*COSB
      RR(2,2,NCODE) = B*SING*SINAS
      RR(3,2,NCODE) = -B*SING*COSAS
      RR(3,3,NCODE) = C*SINB
C
C---- copy rr(...) into rrr(...)
C
      DO 11 I=1,6
        CEL(I) = CELL(I)
11    CONTINUE
      DO 70 I = 1,6
        DO 60 J = 1,3
          DO 50 K = 1,3
            RRR(K,J,I) = RR(K,J,I)
   50     CONTINUE
   60   CONTINUE
   70 CONTINUE
C
C
      VOLL = VOL
      IF (CELDEL.GT.0.01) VOLL = -VOL
C
C---- Format statements
C
 6000 FORMAT (' IN RBFROR ',6F8.3)
C
C
      END
C
C
C
      SUBROUTINE WBROOK(IOUT,ISER,ATNAM,RESNAM,CHNNAM,IRESN,IS,
     *X,Y,Z,OCC,B,IZ)
C     =========================================================
C
C_BEGIN_WBROOK
C
C    THE_SUBROUTINE_'WBROOK'
C                             
C      The subroutine  WBROOK  is  used  to  write  a  coordinate  record  in
C Brookhaven format. It is  intended  to  be  paired  with  the  use  of  the
C subroutine RBROOK as some information  from  the  input  record  (e.g.  the
C footnote number and the alternate location identifier)  is  passed  between
C the routines via the common block /RBRKYY/ and cannot be passed  to  WBROOK
C in the parameter list. If WBROOK is to be used without a record being  read
C via RBROOK, then ITYP in the common  /RBRKXX/  must  be  set  to  0  before
C calling the subroutine.
C                        
C Call:  CALL WBROOK(IOUT,ISER,ATNAM,RESNAM,CHNNAM,IRESN,IS,X,Y,Z,OCC,B,IZ)
C                                                                          
C
C PARAMETERS
C
C        IOUT (I) UNIT NO. OF THE OUTPUT COORDINATE FILE
C        ISER (I) ATOM SERIAL NUMBER
C       ATNAM (I) ATOM NAME (CHARACTER*4 LEFT JUSTIFIED)
C      RESNAM (I) RESIDUE NAME  (CHARACTER*4)
C      CHNNAM (I) CHAIN CODE (CHARACTER*1)
C       IRESN (I) RESIDUE NUMBER AS AN INTEGER (MAX. OF 4 DIGITS)
C                  (IF 0 THEN RESIDUE NUMBER TOGETHER WITH INSERTION
C                  CODE, IF PRESENT, WILL BE COPIED FROM LAST RECORD
C                  READ VIA 'RBROOK')
C          IS (I) RESERVED FLAG (EJD) NOT PART OF BROOKHAVEN DEFN.
C           X (I) COORDINATES (STANDARD ORTHOGONAL)
C           Y (I)     "
C           Z (I)     "
C         OCC (I) OCCUPANCY
C           B (I) TEMPERATURE FACTOR
C          IZ (I) ATOMIC NUMBER (MAY BE 0 IF THE ATOMIC SYMBOL IS A
C                                SINGLE CHARACTER E.G. C,N,O,H,S)
C
C COMMON AREAS: /RBRKXX/,/RBRKYY/
C
C      COMMON /RBRKXX/IFCRYS,IFSCAL,IFEND,ITYP,MATRIX
C      COMMON /RBRKYY/BROOK(80)
C
C_END_WBROOK
C
C
      LOGICAL IFCRYS,IFSCAL,IFEND,MATRIX
      CHARACTER*1 ALTLOC,CHNNAM
      CHARACTER*3 FOOTN,FTNOT
      DIMENSION IAT(15)
      CHARACTER*4 ATNAM,RESNAM
      COMMON /RBRKXX/IFCRYS,IFSCAL,IFEND,ITYP,MATRIX
      CHARACTER*1 BROOK
      COMMON /RBRKYY/BROOK(80)
      EQUIVALENCE (FTNOT,BROOK(68))
      CHARACTER*6 ITYPE(5)
      SAVE /RBRKXX/,/RBRKYY/
      DATA IAT/0,1,5,6,7,8,9,15,16,19,23,39,53,74,92/
      DATA ITYPE/'CRYST1','SCALE ','TER   ','ATOM  ','HETATM'/
C
C---- Choose output format to give correct position for atom name
C     and option for residue number treatment
C
      JTYP=ITYP
      IF(IZ.EQ.0)FTNOT='   '
C      ALTLOC=BROOK(17)
      ALTLOC=ATNAM(4:4)
      FOOTN=FTNOT
      IF(IZ.GT.0)WRITE(FOOTN,9876)IZ
9876  FORMAT(1X,I2)
      IF(ITYP.EQ.0)THEN
      JTYP=4
      ALTLOC=' '
      FOOTN='   '
      ENDIF
      II=0
      IF(IZ.EQ.0)GO TO 50
      II=1
      IF(IZ.NE.1)GO TO 35
      IF(ATNAM(1:1).EQ.'H')II=0
      GO TO 50
35    DO 40 I=1,15
      IF(IZ.EQ.IAT(I))II=0
40    CONTINUE
50    IF(IRESN.NE.0)GO TO 60
      IF(II.EQ.0)WRITE(IOUT,1001)ITYPE(JTYP),ISER,ATNAM,ALTLOC,RESNAM,
     *CHNNAM,(BROOK(I),I=23,27),IS,X,Y,Z,OCC,B,FOOTN
      IF(II.EQ.1)WRITE(IOUT,1002)ITYPE(JTYP),ISER,ATNAM,ALTLOC,RESNAM,
     *CHNNAM,(BROOK(I),I=23,27),IS,X,Y,Z,OCC,B,FOOTN
      RETURN
60    IF(II.EQ.0)WRITE(IOUT,1003)ITYPE(JTYP),ISER,ATNAM,ALTLOC,RESNAM,
     *CHNNAM,IRESN,IS,X,Y,Z,OCC,B,FOOTN
      IF(II.EQ.1)WRITE(IOUT,1004)ITYPE(JTYP),ISER,ATNAM,ALTLOC,RESNAM,
     *CHNNAM,IRESN,IS,X,Y,Z,OCC,B,FOOTN
      RETURN
C
C---- Format statements
C
1001  FORMAT(A6,I5,1X,1X,A3,A1,A3,1X,A1,4A1,A1,I2,1X,3F8.3,2F6.2,1X,A3)
1002  FORMAT(A6,I5,1X,A4,A1,A3,1X,A1,4A1,A1,I2,1X,3F8.3,2F6.2,1X,A3)
1003  FORMAT(A6,I5,1X,1X,A3,A1,A3,1X,A1,I4,1X,I2,1X,3F8.3,2F6.2,1X,A3)
1004  FORMAT(A6,I5,1X,A4,A1,A3,1X,A1,I4,1X,I2,1X,3F8.3,2F6.2,1X,A3)
      END
C
C
C
      SUBROUTINE RWBFIN(IUN,IOUT)
C     ==========================
C
C_BEGIN_RWBFIN
C
C      SUBROUTINE RWBFIN(IUN,IOUT)
C
C
C This subroutine is used to copy the remaining records of a brookhaven
C coordinate file from an input to an output file. It will normally
C be used after part of the file has been copied via RBROOK and
C WBROOK  but may be used to copy a complete file after calling RBINIT.
C
C
C PARAMETERS
C
C         IUN (I) (INTEGER) UNIT NO. OF INPUT COORDINATE FILE
C        IOUT (I) (INTEGER) UNIT NO. OF OUTPUT COORDINATE FILE
C
C Common blocks
C
C      COMMON /RBRKYY/BROOK(80)
C
C_END_RWBFIN
C
      LOGICAL IFCRYS,IFSCAL,IFEND,MATRIX
      COMMON /RBRKXX/IFCRYS,IFSCAL,IFEND,ITYP,MATRIX
      CHARACTER*1 BROOK
      COMMON /RBRKYY/BROOK(80)
      SAVE /RBRKXX/,/RBRKYY/
C
C---- Copy records unless end of file already reached (via 'rbrook')
C
      IF(IFEND)RETURN
10    READ(IUN,1001,END=100)BROOK
      WRITE(IOUT,1001)BROOK
      GO TO 10
100   IFEND=.TRUE.
      RETURN
C
C---- Format statements
C
1001  FORMAT(80A1)
      END
C
C
C
      SUBROUTINE CVFRAC(X,Y,Z,XX,YY,ZZ,IFLAG,MSG)
C     ===========================================
C
C_BEGIN_CVFRAC
C
C      SUBROUTINE CVFRAC(X,Y,Z,XX,YY,ZZ,IFLAG,MSG)
C
C      This subroutine is used to convert between the stored  orthogonal  and
C fractional coordinates using the  matrices  set  up  in  the  common  block
C /ORTHOG/ by the subroutine RBROOK. If no matrices have been set up then the
C program will stop with an error message.
C                                         
C Call:  CALL CVFRAC(X,Y,Z,XX,YY,ZZ,IFLAG,MSG)
C                                             
C Arguments:
C            
C       X (I)   (REAL)  Input coordinates.
C       Y (I)   (REAL)       "
C       Z (I)   (REAL)       "
C      XX (O)   (REAL)  Output coordinates.
C      YY (O)   (REAL)       "
C      ZZ (O)   (REAL)       "
C   IFLAG (I) (INTEGER)  Flag =0, Convert coordinates from fractional to orthogonal
C                             =1, Convert coordinates from orthogonal to fractional
C     MSG (I) (INTEGER)  Unit number for printing error message.
C
C_END_CVFRAC
C
      LOGICAL IFCRYS,IFSCAL,IFEND,MATRIX
      COMMON /RBRKXX/IFCRYS,IFSCAL,IFEND,ITYP,MATRIX
      COMMON /ORTHOG/RO(4,4),RF(4,4),NCODE,IBRKFL
      SAVE /RBRKXX/,/ORTHOG/
C
C---- Check that matrices set up
C
      IF(.NOT.MATRIX)GO TO 800
C
C---- Perform transformation
C
      IF(IFLAG.NE.0)GO TO 10
      XX=RO(1,1)*X + RO(1,2)*Y +RO(1,3)*Z +RO(1,4)
      YY=RO(2,1)*X + RO(2,2)*Y +RO(2,3)*Z +RO(2,4)
      ZZ=RO(3,1)*X + RO(3,2)*Y +RO(3,3)*Z +RO(3,4)
      RETURN
10    XX=RF(1,1)*X + RF(1,2)*Y +RF(1,3)*Z +RF(1,4)
      YY=RF(2,1)*X + RF(2,2)*Y +RF(2,3)*Z +RF(2,4)
      ZZ=RF(3,1)*X + RF(3,2)*Y +RF(3,3)*Z +RF(3,4)
      RETURN
C
C---- Error condition
C
800   IF(MSG.GT.0)WRITE(MSG,2001)
      call ccperr(1,' No knowledge of input orthogonalisation')
C
C---- Format statements
C
2001  FORMAT(' **FRACTIONAL/ORTHOGONAL MATRICES NOT SET UP**')
      END
C
C
C
      SUBROUTINE RBRCEL(RCEL,RVOL)
C     ============================
C
C_BEGIN_RBRCEL
C
C      SUBROUTINE RBRCEL(RCEL,RVOL)
C
C THIS SUBROUTINE RETURNS Reciprocal cell dimensions, and reciprocal
C                       unit cell  volume.
C
C PARAMETERS
C     RCEL (O)  (REAL(6)) reciprocal cell dimensions
C     RVOL (O)  (REAL)    reciprocal cell volume
C
C Common blocks
C
C      COMMON/RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
C
C_END_RBRCEL
C
      COMMON/RBRKZZ/CELL(6),RR(3,3,6),VOL,CELLAS(6)
      DIMENSION RCEL(6)
      SAVE /RBRKZZ/
C
      RVOL = 1.0/VOL
      DO 1 I=1,6
1     RCEL(I) = CELLAS(I) 
      RETURN
      END
C
C
C
      SUBROUTINE RES3TO1(RESNM3,RESNM1)
C     ================================
C
C_BEGIN_RES3TO1
C
C      SUBROUTINE RES3TO1(RESNM3,RESNM1)
C
C       FIND 3 CHARACTER RESIDUE NAME FROM 1 CHARACTER CODE OR
C       FIND 1 CHARACTER RESIDUE NAME FROM 3 CHARACTER CODE.
C       SUBROUTINE IS CALLED WITH EITHER RESNM3 OR RESNM1 PREVIOUSLY 
C       ASSIGNED, AND THE OTHER IS ASSIGNED  HERE.
C 
C Parameters
C
C   RESNM3 (I/O)  CHAR*4    3 character residue name
C   RESNM1 (I/O)  CHAR*1    1 character residue name
C
C_END_RES3TO1
C
      CHARACTER*4 RESNM3
      CHARACTER*1 RESNM1
      CHARACTER*4 MAACD3(26)
      CHARACTER*1 MAACD1(26)
      DATA NAACID/26/
      DATA MAACD3/'ALA ','ARG ','ASN ','ASP ','CYS ','CYH ','GLN ',
     1 'GLU ','GLY ','HIS ','ILE ','LEU ','LYS ','MET ','PHE ','PRO ',
     2 'SER ','THR ','TRP ','TYR ','VAL ','HEM ','WAT ','SUL ','END ',
     3 'DUM '/
      DATA MAACD1/'A','R','N','D','C','C','Q',
     1 'E','G','H','I','L','K','M','F','P',
     2 'S','T','W','Y','V','X','O','U','Z','Z'/
C
C---- Routine to find one character amino acid name
C
        IF(RESNM3.NE.' ')THEN
          DO 1 I=1,NAACID
          IF(RESNM3.EQ.MAACD3(I)) GO TO 2
1         CONTINUE
        I=NAACID
2       RESNM1=MAACD1(I)
        RETURN
        ENDIF
C
C---- Routine to find three character amino acid name
C
        IF(RESNM1.NE.' ')THEN
          DO 11 I=1,NAACID
          IF(RESNM1.EQ.MAACD1(I)) GO TO 12
11        CONTINUE
        I=NAACID
12      RESNM3=MAACD3(I)
        RETURN
        ENDIF
      END
C
C
C
        SUBROUTINE RBRECIP(IH,IK,IL,S)
C       ==============================
C
C_BEGIN_BRECIP
C
C        SUBROUTINE RBRECIP(IH,IK,IL,S)
C
C---- This subroutine calculates 4SIN**2/L**2
C
C PARAMETERS
C         IH,IK,IL (I) (INTEGER)  reflection indices
C                S (O) (REAL)     4SIN**2/L**2
C
C_END_BRECIP
C
      COMMON /RBREC/AC(6)
      SAVE /RBREC/
C
      S = 
     .(AC(1)*IH*IH+AC(2)*IK*IK+AC(3)*IL*IL
     .+AC(4)*IK*IL+AC(5)*IL*IH+AC(6)*IH*IK)
      RETURN
      END

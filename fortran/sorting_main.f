      INTEGER FUNCTION SRTBEG(NKEYS,KEYBUF,NRECL,ISS)
C
      IMPLICIT NONE
      INTEGER NMAX_MEM
      PARAMETER (NMAX_MEM = 8000000)
      INTEGER NKEYS,NRECL,KEYBUF(*),ISS
C
C----Common block for records to be sorted. At the moment
C----no memory allocation is used. It could be changed by switching to 
C----C or using FORTRAN 90 or using calling subroutine supplied 
C----memory. In fortran last variant seems to be better. Then
c----calling routine has to decide how much memory it needs.
      INTEGER NKEYS_S,NRECORD_S,KEY_ADDRESS,RECORD_ADDRESS,
     &        NRECORD_NOW,NRECORD_RETURN,INDEX_ADDRESS,
     &        NRECORD_IN_RUNS,NRECORD_IN_THIS_RUN,
     &        NMAX_RECORD
      INTEGER SAVE_KEYS(5),SAVE_RECORD(200),ASCEND_DESCEND(5)
      REAL LARGE_MEM(NMAX_MEM)
      COMMON /SORT_MEMORY/NMAX_RECORD,NRECORD_IN_RUNS,
     &       NRECORD_IN_THIS_RUN,NKEYS_S,NRECORD_S,
     &       NRECORD_RETURN,NRECORD_NOW,SAVE_KEYS,
     &       SAVE_RECORD,ASCEND_DESCEND,
     &       KEY_ADDRESS,RECORD_ADDRESS,INDEX_ADDRESS,LARGE_MEM
      SAVE /SORT_MEMORY/
C
      INTEGER IK,IREC,J,IRECORD_POSITIONS,NRECL_1
C
C----Find out how many records could be sorted. We need memory for
C----NRECL = NKEYS + NRECORDS number of fields and one field for
C----index of records which is going to be sorted.
      NRECL_1 = NRECL/4
      NMAX_RECORD = NMAX_MEM/(NRECL_1+1)
C
      IF(NMAX_RECORD.LE.1) THEN
        SRTBEG = 1
C
C---Sorting can not be done
      ENDIF
C
C----Save positions of keys and remaining records in an array
C----It would be easier if keybuf would have only info about 
C----ascending or descending and position of key. As it stands
C----it is a bit more comlicated to derive. To minimise 
C----change in the calling subroutine use previous binsort style
C----keybuf. 
      DO   IK=1,NKEYS
         J = (IK-1)*5+1
        SAVE_KEYS(IK) = KEYBUF(J+2)/4 + 1
        ASCEND_DESCEND(IK) = 1
        IF(KEYBUF(J+1).NE.0) ASCEND_DESCEND(IK) = -1
      ENDDO
      NKEYS_S = NKEYS
      IRECORD_POSITIONS = 0
      NRECORD_S = 0
      DO    IREC=1,NRECL_1
         DO   IK=1,NKEYS
            IF(SAVE_KEYS(IK).EQ.IREC) GOTO 100
         ENDDO
         IRECORD_POSITIONS = IRECORD_POSITIONS + 1
         SAVE_RECORD(IRECORD_POSITIONS) = IREC
         NRECORD_S = NRECORD_S + 1
 100     CONTINUE
      ENDDO
C
C---Now we have record numbers as well as key numbers. Define memory 
C---for key and records
      KEY_ADDRESS = 1
      RECORD_ADDRESS = KEY_ADDRESS + NKEYS_S*NMAX_RECORD
      INDEX_ADDRESS = RECORD_ADDRESS + NRECORD_S*NMAX_RECORD
C
C---Now we are ready to accept records. It will be done by another 
C---routine.
C---If external merging is necessary it should be initialised here.
C
      NRECORD_NOW = 0
      NRECORD_RETURN = 0
      SRTBEG = 0
      RETURN
      END
C
      INTEGER FUNCTION SRTRLS(ADATA)
      IMPLICIT NONE
      INTEGER NMAX_MEM
      PARAMETER (NMAX_MEM = 8000000)
C
C----Recieve one record and save it in the list.
C----For large number of records if number of current records
C----is equal to maximum number of records then they should be 
C----sorted and then written to external files taken care of
C----distribution. (polyphase merging is possible option to use)
      REAL ADATA(*)
C
C----Common block for records to be sorted. At the moment
C----no memory allocation is used. It could be changed by switching to 
C----C or using FORTRAN 90 or using calling subroutine supplied 
C----memory. In fortran last variant seems to be better. Then
c----calling routine has to decide how much memory it needs.
      INTEGER NKEYS_S,NRECORD_S,KEY_ADDRESS,RECORD_ADDRESS,
     &        NRECORD_NOW,NRECORD_RETURN,INDEX_ADDRESS,
     &        NRECORD_IN_RUNS,NRECORD_IN_THIS_RUN,
     &        NMAX_RECORD
      INTEGER SAVE_KEYS(5),SAVE_RECORD(200),ASCEND_DESCEND(5)
      REAL LARGE_MEM(NMAX_MEM)
      COMMON /SORT_MEMORY/NMAX_RECORD,NRECORD_IN_RUNS,
     &       NRECORD_IN_THIS_RUN,NKEYS_S,NRECORD_S,
     &       NRECORD_RETURN,NRECORD_NOW,SAVE_KEYS,
     &       SAVE_RECORD,ASCEND_DESCEND,
     &       KEY_ADDRESS,RECORD_ADDRESS,INDEX_ADDRESS,LARGE_MEM
      SAVE /SORT_MEMORY/
C
      INTEGER IKEY_NOW,IREC_NOW,IK,IR
C
C---First save keys.
      NRECORD_NOW = NRECORD_NOW + 1
      IF(NRECORD_NOW.GT.NMAX_RECORD) THEN
         WRITE(*,*)'Too many records'
         SRTRLS = 1
         RETURN
      ELSEIF(NRECORD_NOW.EQ.NMAX_RECORD) THEN
C
C---Memory is not enough for internal sorting. External sorting
C---part should be written. In that case available records 
C---should be sorted and written to file with distribution
c---which is going to be used. (polyphase distribution is one possibility)
C

      ENDIF

      IKEY_NOW = (NRECORD_NOW-1)*NKEYS_S
      DO   IK=1,NKEYS_S
        LARGE_MEM(IKEY_NOW+IK) = ADATA(SAVE_KEYS(IK))*ASCEND_DESCEND(IK)
      ENDDO
      IREC_NOW = RECORD_ADDRESS + (NRECORD_NOW-1)*NRECORD_S - 1
      DO  IR=1,NRECORD_S
         LARGE_MEM(IREC_NOW + IR) = ADATA(SAVE_RECORD(IR))
      ENDDO
      LARGE_MEM(INDEX_ADDRESS+NRECORD_NOW-1) = NRECORD_NOW
C
C---Normal return. No disaster.
      SRTRLS = 0
      RETURN
      END
C
      INTEGER FUNCTION SRTMRG()
C
      IMPLICIT NONE
      INTEGER NMAX_MEM
      PARAMETER (NMAX_MEM = 8000000)
C
C---This function should do merging. But here we use only sorting
C---It will have to be expanded for merging for large number of records
C
C----Common block for records to be sorted. At the moment
C----no memory allocation is used. It could be changed by switching to 
C----C or using FORTRAN 90 or using calling subroutine supplied 
C----memory. In fortran last variant seems to be better. Then
c----calling routine has to decide how much memory it needs.
      INTEGER NKEYS_S,NRECORD_S,KEY_ADDRESS,RECORD_ADDRESS,
     &        NRECORD_NOW,NRECORD_RETURN,INDEX_ADDRESS,
     &        NRECORD_IN_RUNS,NRECORD_IN_THIS_RUN,
     &        NMAX_RECORD
      INTEGER SAVE_KEYS(5),SAVE_RECORD(200),ASCEND_DESCEND(5)
      REAL LARGE_MEM(NMAX_MEM)
      COMMON /SORT_MEMORY/NMAX_RECORD,NRECORD_IN_RUNS,
     &       NRECORD_IN_THIS_RUN,NKEYS_S,NRECORD_S,
     &       NRECORD_RETURN,NRECORD_NOW,SAVE_KEYS,
     &       SAVE_RECORD,ASCEND_DESCEND,
     &       KEY_ADDRESS,RECORD_ADDRESS,INDEX_ADDRESS,LARGE_MEM
      SAVE /SORT_MEMORY/
      INTEGER I,IR,IK

      CALL HEAP_SORT(NRECORD_NOW,NKEYS_S,LARGE_MEM(1),
     &    LARGE_MEM(INDEX_ADDRESS))
C
C---Records have been sorted. They should be distributed. 
C---But it is next stage
      SRTMRG = 0
      RETURN
      END
C
      INTEGER FUNCTION SRTRET(ADATA)
      IMPLICIT NONE
      INTEGER NMAX_MEM
      PARAMETER (NMAX_MEM = 8000000)
C
C----Retrieve next record from the sorted list of the records.
      REAL ADATA(*)
C
C---This function should do merging. But here we use only sorting
C---It will have to be expanded for merging for large number of records
C
C----Common block for records to be sorted. At the moment
C----no memory allocation is used. It could be changed by switching to 
C----C or using FORTRAN 90 or using calling subroutine supplied 
C----memory. In fortran last variant seems to be better. Then
c----calling routine has to decide how much memory it needs.
      INTEGER NKEYS_S,NRECORD_S,KEY_ADDRESS,RECORD_ADDRESS,
     &        NRECORD_NOW,NRECORD_RETURN,INDEX_ADDRESS,
     &        NRECORD_IN_RUNS,NRECORD_IN_THIS_RUN,
     &        NMAX_RECORD
      INTEGER SAVE_KEYS(5),SAVE_RECORD(200),ASCEND_DESCEND(5)
      REAL LARGE_MEM(NMAX_MEM)
      COMMON /SORT_MEMORY/NMAX_RECORD,NRECORD_IN_RUNS,
     &       NRECORD_IN_THIS_RUN,NKEYS_S,NRECORD_S,
     &       NRECORD_RETURN,NRECORD_NOW,SAVE_KEYS,
     &       SAVE_RECORD,ASCEND_DESCEND,
     &       KEY_ADDRESS,RECORD_ADDRESS,INDEX_ADDRESS,LARGE_MEM
      SAVE /SORT_MEMORY/
C
      INTEGER IK,IR,IKEY_REC,IREC_R
C
C----Take keys first.
      NRECORD_RETURN = NRECORD_RETURN + 1
      IF(NRECORD_RETURN.GT.NRECORD_NOW) THEN
C
C----In case of external search read from file new set of records
C----and then check if everything is o.k
C
C---Distaster. Calling subroutine wants more than it has given.
         SRTRET = -1
         RETURN
      ENDIF
C
C---Take keys.
      IKEY_REC = (NRECORD_RETURN-1)*NKEYS_S
      DO   IK=1,NKEYS_S
        ADATA(SAVE_KEYS(IK)) = LARGE_MEM(IKEY_REC + SAVE_KEYS(IK))*
     &                     ASCEND_DESCEND(IK)
      ENDDO
C
C--Now take records.
      IREC_R = RECORD_ADDRESS +
     &    (INT(LARGE_MEM(INDEX_ADDRESS+NRECORD_RETURN-1))-1)*NRECORD_S-1
      DO   IR=1,NRECORD_S
        ADATA(SAVE_RECORD(IR)) = LARGE_MEM(IREC_R +  IR)
      ENDDO
C
C---Succesful retrieval
      SRTRET = 0
      RETURN
      END
C
C---Internal sorting part. It could be improved
C-----------
      SUBROUTINE  HEAP_SORT(N,NKEYS,A_KEY,INDEX_R)
      IMPLICIT NONE
C
C----Sorting using heapsort. 
C----A        contains keys
C----INDEX_R  index of records to be sorted
C----N        number of records
C----NKEYS    number of keys
C
C---Reference
C
c---Knuth, The art of computer programming Volume 3
C---1998
C
      INTEGER    N,NKEYS
      REAL       INDEX_R(*)
      REAL       A_KEY(*)
C
      REAL    T_KEY(5),T1_KEY(5)
C
      REAL      INDEX_C
      INTEGER   L,M,I_TEMP,L1,MKEY1_ADD,MKEY2_ADD,I_KEY,N1
C
C--------------------------------------------------------------
      L1 = N/2
C
C---Create heap. everybody tends to reach his level of incomptence
      WRITE(*,*)'In heap_sort'

      DO   L=L1,1,-1
        CALL SIFT_UP(N,L,NKEYS,A_KEY,INDEX_R)
      ENDDO
C
c--Remove heaps  one after another
      N1 = N-1
      DO   M=N1,1,-1
        INDEX_C = INDEX_R(1)
        INDEX_R(1) = INDEX_R(M+1)
        INDEX_R(M+1) = INDEX_C
        MKEY1_ADD = M*NKEYS
        DO   I_KEY=1,NKEYS
          MKEY2_ADD = MKEY1_ADD + I_KEY
          T_KEY(I_KEY) = A_KEY(I_KEY)
          A_KEY(I_KEY) = A_KEY(MKEY2_ADD)
          A_KEY(MKEY2_ADD) = T_KEY(I_KEY)
        ENDDO

        CALL SIFT_UP(M,1,NKEYS,A_KEY,INDEX_R)
      ENDDO
      END
C
      SUBROUTINE SIFT_UP(M,L,NKEYS,A_KEY,INDEX_R)
c
c---Sift ip process in heap sort. This implementation is 
C---intended to work for multykey cases. I am not sure this treatment is best
C---for multy key cases. Other idea will have to be tested.
      IMPLICIT NONE
      INTEGER M,L,NKEYS
      REAL INDEX_R(*)
      REAL A_KEY(*)
C
      INTEGER I,J,IKEY_ADD,NKEYS1,JKEY_ADD,JKEY1_ADD,LKEY_ADD,
     &        I_KEY,J_KEY,I1_KEY
      REAL KEY_T(5),INDEX_C
C
C---Intitialise sift up (in Knuth's terminology H2)
      INDEX_C = INDEX_R(L)
      NKEYS1  = -NKEYS
      LKEY_ADD = L*NKEYS+NKEYS1
      DO  I_KEY=1,NKEYS
        KEY_T(I_KEY) = A_KEY(LKEY_ADD + I_KEY)
      ENDDO
c
C---H3
      J = L
 40   CONTINUE
C
c---Start sift up. Compare I with 2*I or 2*I+1
C---H4
      I = J
      J = J + J
C
C---If(J.GT.M) then return time
      IKEY_ADD = I*NKEYS + NKEYS1
      JKEY_ADD = J*NKEYS + NKEYS1
      IF(J.LE.M) THEN
         IF(J.LT.M) THEN
C
c---J.lt.M find maximum of J and J + 1 (which is 2*I, 2*I+1)
           JKEY1_ADD = JKEY_ADD + NKEYS
C
C---H5 
           DO   I_KEY=1,NKEYS
             IF(A_KEY(JKEY_ADD+I_KEY).LT.A_KEY(JKEY1_ADD+I_KEY)) THEN
C
c---Next record is smaller, Take it
               J = J + 1
               JKEY_ADD = JKEY1_ADD
               GOTO 55
             ELSE IF(A_KEY(JKEY_ADD+I_KEY).GT.
     &                                A_KEY(JKEY1_ADD+I_KEY))THEN
C
c--Next record is greater. No need to proceed
               GOTO 55
             ENDIF
C
c----Cannot decide yet. Check another key
           ENDDO
         ENDIF
 55      CONTINUE
         DO   I_KEY = 1,NKEYS
C
C---compare saved record in initialisation with record J. If
C---saved is smaller than current then then put for i (2*I or 2*I+1)
           IF(KEY_T(I_KEY).LT.A_KEY(JKEY_ADD + I_KEY)) THEN
C
C---H6 above H7 below
             INDEX_R(I) = INDEX_R(J)
             DO   I1_KEY=1,NKEYS
               A_KEY(IKEY_ADD+I1_KEY) = A_KEY(JKEY_ADD+I1_KEY)
             ENDDO
             GOTO 40
           ELSE IF(KEY_T(I_KEY).GT.A_KEY(JKEY_ADD + I_KEY)) THEN
             GOTO 80
           ENDIF
         ENDDO
       ENDIF
 80    CONTINUE
C
c---Put saved record to i-th and exit from sift up
C---H8
       INDEX_R(I) = INDEX_C
       DO   I_KEY=1,NKEYS
         A_KEY(IKEY_ADD+I_KEY) = KEY_T(I_KEY)
       ENDDO
       RETURN
       END

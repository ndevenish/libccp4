C
C     .. Parameters ..
      INTEGER MaxDeriv
      PARAMETER (MaxDeriv=11)
C     ..
      REAL R1
      INTEGER NS
      CHARACTER CC*15,SS*15
      INTEGER DerPointer,ND
      COMMON /CMIRder/SS(MaxDeriv),CC(MaxDeriv)
      COMMON /MIRder/ND,R1(2,MaxDeriv),NS(MaxDeriv),DerPointer
C     ..

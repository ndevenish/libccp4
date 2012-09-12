C     .. Parameters ..
      INTEGER MaxSymmetry
      PARAMETER (MaxSymmetry=192)
      INTEGER MaxNonStand
      PARAMETER (MaxNonStand=13)
      INTEGER MaxStandard
      PARAMETER (MaxStandard=230)
      INTEGER MaxTokens
      PARAMETER (MaxTokens=200)
C     ..
      REAL Fvalue(MaxTokens)
      INTEGER Ibeg(MaxTokens),Idec(MaxTokens),Iend(MaxTokens),
     +        Ityp(MaxTokens),KnownNonStnd(MaxNonStand)
      CHARACTER Cvalue(MaxTokens)*4,SGstndName(MaxStandard)*20,
     +          EquivPos(MaxSymmetry)*50,NonStnd(MaxNonStand)*80
      CHARACTER PosXyz(MaxSymmetry)*80
C     ..

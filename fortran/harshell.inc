      INTEGER MaxRshells
      PARAMETER (MaxRshells = 100)
      INTEGER NRshells,Nreflns(4,MaxRshells),
     +               Nfpb(MaxRshells)
      REAL    Res(2,MaxRshells),Rs(7,MaxRshells),
     +               sds(2,MaxRshells),rmult(MaxRshells),
     +               Poss(3,MaxRshells),FPB(MaxRshells)
      COMMON /Rshell/NRshells,Res,
     +               Nreflns,Rs,
     +               sds,rmult,
     +               Poss,FPB,
     +               Nfpb
C     ..

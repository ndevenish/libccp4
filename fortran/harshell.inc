      INTEGER MaxRshells
      PARAMETER (MaxRshells = 100)
      INTEGER NRshells,Nreflns,Nfpb
      REAL Res,Rs,sds,rmult,Poss,FPB
      COMMON /Rshell/NRshells,Res(2,MaxRshells),
     +               Nreflns(4,MaxRshells),Rs(7,MaxRshells),
     +               sds(2,MaxRshells),rmult(MaxRshells),
     +               Poss(3,MaxRshells),FPB(MaxRshells),
     +               Nfpb(MaxRshells)
C     ..

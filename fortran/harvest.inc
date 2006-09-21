      implicit none

C     .. Parameters ..
      INTEGER AppendRow,KeepContext
      PARAMETER (AppendRow=3,KeepContext=1)
      INTEGER MaxLines
      PARAMETER (MaxLines = 10)
C
C     .. Common blocks ..
      INTEGER rom_context,restraints_context,MirDerSiteID,
     +        EnvRet
      LOGICAL Harvest
      INTEGER ccifBlockID,ccifBlocks,ccifContext,ccifStatus,
     +        IValueNotDet
      REAL ValueNotDet
      CHARACTER PName*20,DName*20,Package*80,SoftwareName*80,
     +          SoftwareVersion*80,CurrBlock*81,CurrCategory*81,
     +          DEPOSITFNAME*256
c
c
      COMMON /ccifOpn/MirDerSiteID,rom_context,restraints_context,
     +                EnvRet
      COMMON /ccifChar/CurrBlock,CurrCategory,PName,DName,Package,
     +                 SoftwareName,SoftwareVersion,DEPOSITFNAME
      COMMON /ccifDat/IValueNotDet,ccifBlocks,
     +       ccifBlockID,ccifStatus,ccifContext,Harvest
      COMMON /Dcif/ValueNotDet
C     ..

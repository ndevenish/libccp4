C     .. Parameters ..
      INTEGER AppendRow,KeepContext
      PARAMETER (AppendRow=3,KeepContext=1)
      INTEGER MaxLines
      PARAMETER (MaxLines = 10)
C
C     .. Common blocks ..
      INTEGER rom_context,restraints_context,MirDerSiteID,
     +        EnvRet
      COMMON /ccifOpn/MirDerSiteID,rom_context,restraints_context,
     +                EnvRet
      CHARACTER PName*20,DName*20,Package*80,SoftwareName*80,
     +          SoftwareVersion*80,CurrBlock*81,CurrCategory*81,
     +          DEPOSITFNAME*256
      COMMON /ccifChar/CurrBlock,CurrCategory,PName,DName,Package,
     +                 SoftwareName,SoftwareVersion,DEPOSITFNAME
      LOGICAL Harvest
      INTEGER ccifBlockID,ccifBlocks,ccifContext,ccifStatus,
     +        IValueNotDet
      COMMON /ccifDat/IValueNotDet,ccifBlocks,
     +       ccifBlockID,ccifStatus,ccifContext,Harvest
      REAL ValueNotDet
      COMMON /Dcif/ValueNotDet
C     ..

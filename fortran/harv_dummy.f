C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
      SUBROUTINE HSymTrn(Nsym,Rsm,Symchs)
C     ==========================================
C
C     .. Parameters ..
      INTEGER MaxSymmetry
      PARAMETER (MaxSymmetry=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER Nsym
C     ..
C     .. Array Arguments ..
      REAL Rsm(4,4,*)
      CHARACTER Symchs(MaxSymmetry)*80
C
      RETURN
      END
      SUBROUTINE HTrnSym(Nsym,Rsm,Symchs)
C     ========================================
C
C     .. Parameters ..
      INTEGER MaxSymmetry
      PARAMETER (MaxSymmetry=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER Nsym
C     ..
C     .. Array Arguments ..
      REAL Rsm(4,4,*)
      CHARACTER Symchs(MaxSymmetry)*80
      RETURN
      END
C
C     =========================================================
      SUBROUTINE Hatom_type_scat(AtName,a1,a2,a3,a4,b1,b2,b3,b4,c)
C     =========================================================

C     .. Array Arguments ..
      CHARACTER AtName*(*)
      REAL a1,a2,a3,a4,b1,b2,b3,b4,c
C
      RETURN
      END
      SUBROUTINE HCell(Cell)
C     ============================

C     .. Array Arguments ..
      REAL Cell(6)
C
      RETURN
      END
C
C
C     ====================================
      SUBROUTINE Hcell_percent_solvent(Fraction)
C     ====================================
C
C     .. Scalar Arguments ..
      REAL Fraction

      RETURN
      END
c     ============================
      subroutine hciftime(ciftime)
c     ============================
c
      character ciftime*(*)
c
      return
      end

C
C
C     =======================
      SUBROUTINE Hclose
C     =======================
C
C
      RETURN
      END
C
C
C     ===============================================
      SUBROUTINE Hdata_reduction_method(Method,Nlines)
C     ================================================
C
C     .. Array Arguments ..
      INTEGER Nlines
      CHARACTER Method(Nlines)*80
C
      RETURN
      END
      SUBROUTINE Hdensity_Matthews(Fraction)
C     ====================================

C     .. Scalar Arguments ..
      REAL Fraction
C
      RETURN
      END
C
C
C     ============================================================
      SUBROUTINE Hinitialise(Pkage,
     +                       ProgName,ProgVersion,
     +                       ProjectName,DataSetName,
     +                       UseCWD,Private,
     +                       IVALND,VALND,RowLimit)
C     ============================================================
C
C     .. Scalar Arguments ..
      REAL VALND
      INTEGER IVALND,RowLimit
      LOGICAL Private,UseCWD
      CHARACTER DataSetName* (*),
     +           ProjectName* (*),
     +            ProgVersion* (*),
     +             ProgName* (*),
     +              Pkage* (*)
C
      RETURN
      END
C
C     ===============================================
      SUBROUTINE Hmerge_reject_criterion(Rcriteria,Nlines)
C     ================================================
C
C     .. Array Arguments ..
      INTEGER Nlines
      CHARACTER Rcriteria(Nlines)*80
C
      RETURN
      END
C
C
C     =====================================================
      SUBROUTINE Hoverall_observations(Ntotal,R1,R2)
C     =====================================================
C
C     .. Array Arguments ..
      REAL R1,R2
      INTEGER Ntotal
C
      RETURN
      END
      SUBROUTINE Hparse(line,Ibeg,Iend,Ityp,Fvalue,Cvalue,Idec,N)
C     ==========================================================
C     ..
C     .. Scalar Arguments ..
      INTEGER N
      CHARACTER line* (*)
C     ..
C     .. Array Arguments ..
      REAL Fvalue(*)
      INTEGER Ibeg(*),Idec(*),Iend(*),Ityp(*)
      CHARACTER Cvalue(*)*4
      RETURN
      END
      SUBROUTINE Hphasing_mir_der_p1(NumDer,DerID,NumSitesDer,
     +                               Criteria,Resomin,Resomax)
C     ===========================================================

C     .. Array Arguments ..
      REAL Resomin(2,*),Resomax(2,*),Criteria(*)
      INTEGER NumSitesDer(*)
      CHARACTER DerID(*)*80
C     ..
C     .. Scalar Arguments ..
      INTEGER NumDer
C
      RETURN
      END
      SUBROUTINE Hphasing_mir_der_p2(Power,RCullis,Reflns)
C     ================================================
C     ..
C     .. Array Arguments ..
      REAL Power(2),RCullis(3)
      INTEGER Reflns(3)

      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE Hphasing_mir_der_site(DerID,NumDerSites,B,Atype,
     +                                  X,Y,Z,Occ,OccEsd,anom,
     +                                  AnomEsd)
C     =============================================================
C
C     .. Array Arguments ..
      INTEGER NumDerSites
      REAL anom(*),AnomEsd(*),B(*),Occ(*),OccEsd(*),X(*),Y(*),Z(*)
      CHARACTER Atype(*)*4, DerID*80
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE Hphasing_mir_native(R1,R2,SigmaNat,
     +                            fomT,fomC,fomA,
     +                            Mt,Mc,Ma)
C     =============================================================
C
C     .. Array Arguments ..
      REAL R1,R2,fomT,fomC,fomA,SigmaNat
      INTEGER Mt,Mc,Ma
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE Hphasing_mir_native_shell(R1,R2,KRa,fomA,
     +                                     KRc,fomC,KRo,fomO)
C     =============================================================
C
C     .. Array Arguments ..
      REAL R1,R2,fomA,fomC,fomO
      INTEGER  KRa,KRc,KRo
C
      RETURN
      END
C
C
C        ====================================================
         SUBROUTINE Hrefine_NparamNrestNconstr(NPARMR,NRESTR,NCONSTR)
C        ====================================================
C
C     .. Array Arguments ..
      INTEGER NPARMR,NRESTR,NCONSTR
C
      RETURN
      END
C
C
C         ================================================
          SUBROUTINE Hrefine_corr_esu(Corr,FreeCorr,
     +                                DPI,FreeESU,
     +                                Good,GoodFree,
     +                                ESUml,bESU)
C         ================================================
C
C     .. Array Arguments ..
      REAL Corr,FreeCorr,DPI,FreeESU,
     +     Good,GoodFree,ESUml,bESU
C
      RETURN
      END
C
C
C     ==========================================
      SUBROUTINE Hrefine_details(NHlines,Hlines)
C     ==========================================
C
C     .. Array Arguments ..
      INTEGER NHlines
      CHARACTER Hlines(NHlines)*80
C
      RETURN
      END
C
C     =========================================================
      SUBROUTINE Hrefine_fnmin(Nval,Nterms,Vterms,Cterms,Wterms)
C     =========================================================
C
C     .. Array Arguments ..
      REAL Vterms(*),Wterms(*)
      INTEGER Nterms(*)
      CHARACTER Cterms(Nval)*80
C
      RETURN
      END
C
C
C        ===================================
         SUBROUTINE Hrefine_fom(fom,Freefom)
C        ===================================
C
C     .. Array Arguments ..
      REAL fom,Freefom
C
      RETURN
      END
C
C
C       =========================================
        SUBROUTINE Hrefine_ls_matrix_type(STRING)
C       =========================================
C
C     .. Scalar Arguments ..
      CHARACTER String*(*)
C
      RETURN
      END
C
C
C
C       =================================================
        SUBROUTINE Hrefine_ls_overall_reso(R1,R2)
C       =================================================

C     .. Array Arguments ..
      REAL R1,R2
C
      RETURN
      END
C
C
C     ============================
      SUBROUTINE Hrefine_obscriteria(Criteria)
C     ============================
C
C     .. Array Arguments ..
      REAL Criteria
C
      RETURN
      END
C
C
C       ==================================================
        SUBROUTINE Hrefine_restraints(Rtype,Num,rmsd,sigd,
     +                                criteria,Nreject)
C       ==================================================
C
C     .. Array Arguments ..
      CHARACTER Rtype*(*),criteria*(*)
      INTEGER   Num,Nreject
      REAL rmsd,sigd
C
      RETURN
      END
C
C
C       ==========================================================
        SUBROUTINE Hrefine_rfacts(Nall,Nobs,Nmiss,Nwork,Nfree,
     +                            NfreeMiss,PercentObs,PercentFree,
     +                            Rall,Robs,Rwork,Rfree,
     +                            Wall,Wobs,Wwork,Wfree)
C       ==========================================================
C
C     .. Array Arguments ..
      INTEGER Nall,Nobs,Nmiss,Nwork,Nfree,
     +                            NfreeMiss
      REAL PercentObs,PercentFree,
     +                            Rall,Robs,Rwork,Rfree,
     +                            Wall,Wobs,Wwork,Wfree
C
      RETURN
      END
C
C
C       ==========================================================
        SUBROUTINE Hrefine_rg(RDhigh,RDlow,RGall,RGwork,RGfree)
C       ==========================================================
C
C     .. Array Arguments ..
      REAL RGall,RGwork,RGfree,RDhigh,RDlow
C
      RETURN
      END
C
C
C       ==================================================
        SUBROUTINE Hrefine_rmsobs2dict(Rtype,Clow,Chigh,Usigma)
C       ==================================================
C
C     .. Array Arguments ..
      CHARACTER Rtype*(*)
      REAL Clow,Chigh,Usigma
C
      RETURN
      END
C
C      ========================================================
       SUBROUTINE Hrefine_shell(Rlow,Rhigh,
     +                          NrefAll,NrefObs,Nmiss,NrefWork,
     +                          PercentObs,RfacAll,RfacObs,
     +                          Nfree,Rfree,Rwork,
     +                          WgtRfac,WgtRobs,WgtRwork,
     +                          WgtUsed,Wexpress)
C      ========================================================
C
C     .. Array Arguments ..
      REAL Rlow,Rhigh,PercentObs,RfacObs,RfacAll,
     +     Rfree,Rwork,WgtRfac,WgtRobs,WgtRwork,
     +     Wgtused,Wexpress
      INTEGER  NrefAll,NrefObs,Nmiss,NrefWork,
     +         Nfree
C
      RETURN
      END
C
C
C     ==========================================
      SUBROUTINE Hrefine_solvent_model(NHlines,Hlines)
C     ==========================================
C
C     .. Array Arguments ..
      INTEGER NHlines
      CHARACTER Hlines(NHlines)*80
C
      RETURN
      END
C
C
C     ============================================================
      SUBROUTINE Hrefine_wghtScheme(WghtScheme)
C     ============================================================
C
C     .. Scalar Arguments ..
      CHARACTER WghtScheme* (*)
C
      RETURN
      END
C
C
C     ==========================================
      SUBROUTINE Hrefine_wght_details(NHlines,Hlines)
C     ==========================================
C
C     .. Array Arguments ..
      INTEGER NHlines
      CHARACTER Hlines(NHlines)*80
C
      RETURN
      END
      SUBROUTINE Hrefln_sys_abs(IH,IK,IL,F,SF)
C     =============================================================
C
C     .. Array Arguments ..
      REAL F,SF
      INTEGER IH,IK,IL
C
      RETURN
      END
C
C
C     ============================
      SUBROUTINE Hreflns(WilsonB,R1,R2,Criteria,
     +                         AMI,AMF,Nref)
C     ============================
C
C     .. Array Arguments ..
      REAL WilsonB,R1,R2,AMI,AMF
      INTEGER Nref
      CHARACTER Criteria* (*)
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE Hreflns_intensity_shell(Z,ACT,ACO,CT,CO)
C     =============================================================
C
C     .. Array Arguments ..
      REAL Z,ACT,ACO,CT,CO
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE  Hreflns_overall_merge_p1(R1,R2,
     +                    nmeas,nuniq,ncent,nano,
     +                    fsigi,Rfac,ranom,sdIsignal,fpbias,Ntb)
C     =============================================================
C
C     .. Array Arguments ..
      INTEGER  nmeas,nuniq,ncent,nano,Ntb
      REAL     R1,R2,fsigi,Rfac,ranom,sdIsignal,fpbias
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE  Hreflns_overall_merge_p2(complt,Rmult,pcv,pcvo,
     +                                     rmeas,rmeaso,anomfrc)
C     =============================================================
C
C     .. Array Arguments ..
      REAL complt,Rmult,pcv,pcvo,rmeas,rmeaso,anomfrc
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE Hreflns_overall_merge_p3(aihmin,aihmax,
     +                              t1f,nssf,t2f,t3f,
     +                              t1p,nssp,t2p,t3p)
C     =============================================================
C
C     .. Array Arguments ..
      INTEGER nssf, nssp
      REAL aihmin,aihmax,t1f,t2f,t3f,t1p,t2p,t3p
C
      RETURN
      END
      SUBROUTINE Hreflns_scaling_shell(R1,R2,Nref,AI,AF)
C     =============================================================
C
C     .. Array Arguments ..
      REAL R1,R2,AI,AF
      INTEGER Nref
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE Hreflns_shell_p1(R1,R2,Nmeas,Nuniq,Ncent,
     +                            Nanom,fsigi,Rfac,Rcum,
     +                            Ranom,sdIsignal,fpbias,Npb)
C     =============================================================
C
C     .. Array Arguments ..
      INTEGER Nmeas,Nuniq,Ncent,Nanom,Npb
      REAL R1,R2,fsigi,Rfac,Rcum,Ranom,sdIsignal,FPbias
C
      RETURN
      END
C
C
C     =============================================================
      SUBROUTINE Hreflns_shell_p2(R1,R2,complt,ccmplt,amltpl,
     +                            pcvo,pcv,
     +                            rmeas,rmeaso,anomfrc)
C     =============================================================
C
C     .. Array Arguments ..
      REAL complt,ccmplt,amltpl,pcvo,pcv,
     +     rmeas,rmeaso,anomfrc,R1,R2
C
      RETURN
      END
C
C
C     ============================================================
      SUBROUTINE Hsoftware(SoftwareClass,
     +                     SoftwareAuthor,
     +                     SoftwareEmail,
     +                     SoftwareDescr)
C     ============================================================
C
C     .. Scalar Arguments ..
      REAL VALND
      INTEGER IVND,RowLimit
      LOGICAL Private,UseCWD
      CHARACTER SoftwareAuthor* (*),
     +           SoftwareEmail* (*),
     +            SoftwareDescr* (*),
     +             SoftwareClass* (*)
C
      RETURN
      END
C
C
C     =========================================================
      SUBROUTINE HSymmetry(IntTabNum,SpaceGrpNam,NumEquiv,
     +                           RFsymm)
C     =========================================================
C
C     .. Array Arguments ..
      REAL RFsymm(4,4,*)
C     ..
C     .. Scalar Arguments ..
      INTEGER IntTabNum,NumEquiv
      CHARACTER SpaceGrpNam*10

      RETURN
      END

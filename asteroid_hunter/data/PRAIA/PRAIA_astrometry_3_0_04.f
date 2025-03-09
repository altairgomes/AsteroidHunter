c
c
c
c     PRAIA_astrometry task
c
c
c     Purpose
c
c
c
c     Given a set of images, identifies catalogue stars and targets in the FOV,
c     determinates measurements (x, y) and raw PSF photometry of objects and
c     sky background, outputs (RA, Dec) positions, magnitudes and errors, and
c     many other information regardging the stars and targets. Everything is
c     done automatically.
c
c
c     The objects are automatically and very fastly identified in the CCD frame
c     by an original spiral search procedure. The regions surrounding the identified
c     objects are fitted by a PSF in order to obtain the photocenter, that is, the
c     (x,y) measurement. If the object is round-shaped, the PSF currently used is
c     a 2-D symetrical Gaussian. In this case, only pixels within 1 FWHM (seeing)
c     from the center are fitted in an iterative procedure. If it is a trace-shaped
c     object, a special PSF is fitted (see comments about this PSF further in the
c     text). The (x,y), PSF magnitude and other parameters from the PSF adjustments,
c     errors, etc, are stored and made available in the PRAIA output files.
c     
c
c
c     The catalog star identification is started with a 4-constant adjustment
c     with and without reflection in X. It is only necessary to furnish an
c     approximate pixel scale with an estimate of its error to carry on
c     the identification automatically. The (say n = 30) brightest measured stars
c     and the (say M = 100) brightest catalogue stars are used for the
c     identification. In the process the pixel scale is improved, as well as the
c     tangent plane polynomial model relating catalogue and measured coordinates,
c     from the 4-constant to the 1rst degree, then second and finally third - if
c     there are enough reference stars available, and regardless of the correct
c     or final model furnished by the user. The pixel scale and other parameters
c     are eventually and precisely determined at the end, after the final (RA,Dec)
c     reduction of the FOV with the tangent plane model choosen by the user.
c
c     In the case that all the images come from the same instrument setup, with
c     the same axis orientation and pixel scale, the user has the option of
c     using the adjusted parameters from the first image reduction (orientation
c     and scale) in the catalogue/star identifications of the remaining images,
c     considerably speeding up the process.
c       
c
c     The identification is now done starting with the stars from the Gaia DR3
c     catalogue. From the (RA,Dec) reduction of the FOV with this catalogue,
c     after obtaining Gaia DR3-based (RA,Dec)s for all the objects in the FOV, the
c     identification of stars from the other catalogs is made by direct comparison
c     with these (RA,Dec)s.
c
c
c     Only FITS images are treated. Littleendian or bigendian, bitpix 16, 32, -32,
c     64, -64 are all accepted. All standard WCS and FITS header keys apply. No
c     external packages are necessary for reading FITS images. PRAIA_astrometry, as
c     the other tasks of the package, is an auto-sufficient stand-alone FORTRAN 77
c     program with subroutines.      
c
c
c     Bad pixel masks can be furnished for each individual image and/or for all
c     the images being proccessed. The format is the same as the line format of IRAF.
c
c
c     In this version, 2MASS stars are extracted using the Vizier query web service.
c
c
c     In this version, Gaia DR3 stars are extracted using the Vizier query web service.
c
c
c     Besides the Gaia DR3 catalogue, the user can also use a "User Catalogue" for
c     obtaining (RA,Dec) in his/her catalogue frame.
c
c
c     (RA,Dec) reductions can also be done with an Ephemeris-based reference catalogue,
c     for the so called (RA,Dec) similitude reductions.
c
c     The user can externally setup CCD regions for the detection of objects,
c     overcoming the automatic object identification. It is particularly useful when
c     the user notices that, for some reason, a target was not identified in a
c     previous run. The files are in DS9 format. PRAIA also outputs files in DS9
c     format (.reg files) indicating the objects measured in the FOV and the region
c     used for the PSF fitting for getting the (x,y) measurements.
c
c  
c     In this version, trace-images (asteroids, NEAs, artifical satellites, meteors)
c     of any length and orientation are automatically detected and measured among
c     round-shaped, stellar-like images (stars, etc) in the FOV. A special analytical
c     PSF model was devised for this purpose. The user can also externally setup
c     CCD regions for the detection of trace-images of any length and orientation
c     in the CCD field, among those for round-shaped objects, in the same file. 
c
c
c     In this version, (RA, Dec) re-reduction from previous (RA,Dec) reductions can
c     be performed, based on previous PRAIA files containing (x,y) measurements or
c     simply previously reduced (RA,Dec)s (tangent plane technique). This incorporates,
c     with improvements, the old separate PRAIA_redo_astrometry task. 
c
c
c     In this version, more FDPs (Field Distortion Pattern) options are available. For
c     instance, it is now allowed to read (x,y) distortion maps directly from the FITS
c     headers from individual CCD images of the DESCAM (DES). Also, FDP can now be
c     passed in polynomial form or from a file in (x,y) or (RA,Dec) units.
c
c
c     In this version, the are now files with target positions output in MPC and NIMA
c     ingestion formats for orbit fitting.
c
c
c     In this version, the 2MASS catalogue is only used for extraction of (J, H, K)
c     infrared magnitudes and errors.
c
c
c     In this version, (RA,Dec) reductions are made with astrometric Gaia DR3 corrected
c     by proper motions only (parallax and radial velocity are not used).
c
c
c
c
c      Last update: Marcelo Assafin - 02/Oct/2022
c   
c
c
*----------------------------------------------------------------------------------
*
*  Copyright (c) 2023 PRAIA Marcelo Assafin
*
*  ======================
*  PRAIA Software License
*  ======================
*
*  Permission is hereby granted, free of charge, to any person obtaining a copy
*  of this software and associated documentation files (the “Software”), to deal
*  in the Software without restriction, including without limitation the rights
*  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
*  of the Software, and to permit persons to whom the Software is furnished to do
*  so, subject to the following conditions:
*      
*  Note:
*
*  1. THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
*  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
*  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
*  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*
*  2. SOFA SOFTWARE USED BY PRAIA IS UNDER SOFA SOFTWARE LICENSE
*
*  3. NUMERICAL RECIPES SOFTWARE USED BY PRAIA IS UNDER NUMERICAL RECIPES SOFTWARE
*  LICENSE
*
*  Correspondence concerning PRAIA software should be addressed as follows:
*
*     Internet email: massaf@ov.ufrj.br   (insert the word "PRAIA" in the subject)
*     Postal address: Marcelo Assafin - PRAIA
*                     Observatorio do Valongo
*                     Ladeira do Pedro Antonio 43 - Saude
*                     20.080-090 Rio de Janeiro, RJ, Brazil
*
*----------------------------------------------------------------------------------
      
     


      IMPLICIT REAL *8 (A-H,O-Z)
      parameter (idin50=50)

      character*1 iver
      character*(idin50) linha



 1    continue

      linha=''

      read (*,2,err=10,end=10) linha
2     format(a50)


      if (linha.eq.'') go to 10


      if (linha(1:1).eq.' ') go to 10


      idiobs=0
      read (linha,*,err=10,end=10) idiobs


      if (idiobs.le.0) go to 10

      read (*,*,err=10,end=10) idigai

      if (idigai.le.0) go to 10
      
      read (*,*,err=10,end=10) idi2ma

      if (idi2ma.le.0) go to 10
      
      read (*,*,err=10,end=10) idimx

      if (idimx.le.0) go to 10
      
      read (*,*,err=10,end=10) idimy

      if (idimy.le.0) go to 10
      
      iddo=idiobs
      
      call main (idiobs,iddo,idigai,idi2ma,idimx,idimy,idin50)

      idiobs=0
      iddo=0
      idigai=0
      idi2ma=0
      idimx=0
      idimy=0


      read (*,*,err=10,end=10) iver

      go to 1
c
      
 10   continue

c

      write (*,*)
      write (*,*)
      write (*,*)
      write (*,*)
      write (*,61) 
 61   format (23x,'PRAIA_astrometry: processing terminated.')
      write (*,*) ' '
      write (*,*) ' '
      write (*,*) ' '


      
      end

c
c
c
      

      subroutine main (idiobs,iddo,idigai,idi2ma,idimx,idimy,idin50)
      
      IMPLICIT REAL *8 (A-H,O-Z)
      
c     parameter (stdin=5,idiobs=300000,idigai=3000000,idi2ma=1000000,
c    ?icofsp=21,idin=150,idin50=50,idin2=20000,jfdp=10000,jjfdp=10,
c    ?linxy=425,idimx=4097,idimy=4097,ncells=3)

      parameter (stdin=5,icofsp=21,idin=150,idin2=20000,jfdp=10000,
     ?jjfdp=10,linxy=425,ncells=3)


      
c     parameter(iddo=idimx*idimy/(ncells*ncells)+1)

c     parameter(iddo=idiobs)


      integer*2 imagem(idimx,idimy)
      real*4 pixmat(idimx,idimy)
      real*4 pixel(idimx,idimy)
      integer*2 bitpix,bitpyx,betpix


c
c     L.S. fitting variables
c

      dimension u(idiobs,icofsp),v(icofsp,icofsp),w(icofsp),cvm(icofsp,
     ?icofsp),wsig(idiobs),z(idiobs),xrray(icofsp,icofsp),yrray(icofsp,
     ?icofsp),array(icofsp,icofsp)


      dimension xest(idiobs),yest(idiobs),xp(idiobs),yp(idiobs),
     ?coefx(idiobs)


c
c     Multiuse auxiliary vairables
c

      dimension contag(idiobs),ior(idiobs),nval(idiobs),ior1(idiobs),
     ?nval1(idiobs),val(idiobs),sv(idiobs)


c
c     BOIA variables
c


      dimension mx(iddo),my(iddo)


      dimension xid(idiobs),yid(idiobs),idlado(idiobs),idx1(idiobs),
     ?idx2(idiobs),idy1(idiobs),idy2(idiobs),npix(idiobs),bcg(idiobs),
     ?bcgs(idiobs),nbcg(idiobs),dan(idiobs),exce(idiobs),sige(idiobs),
     ?uu20(idiobs),uu02(idiobs),uu11(idiobs),snr(idiobs),cra(idiobs),
     ?cann(idiobs),cf(idiobs),seg1(idiobs),seg2(idiobs),marca(idiobs)



c
c     (x,y) measuring variables
c

 
      dimension xcir(idiobs),ycir(idiobs),lacir(idiobs),bcgc(idiobs),
     ?bcgsc(idiobs),ixb(idiobs),iyb(idiobs)

      dimension xtra(idiobs),ytra(idiobs),xlatra(idiobs),ylatra(idiobs),
     ?angtra(idiobs),bcgtr(idiobs),bcgstr(idiobs)
   
      dimension volum(idiobs),angle(idiobs),ng(5)

      dimension expix(idiobs),eypix(idiobs),exgai(idiobs),eygai(idiobs),
     ?exusr(idiobs),eyusr(idiobs),exeph(idiobs),eyeph(idiobs)


      dimension xob(idiobs),yob(idiobs),xoob(idiobs),yoob(idiobs),
     ?ilado(idiobs),seng(idiobs),iflag(idiobs),altu(idiobs),
     ?ialtu(idiobs)


      dimension sgcc(idiobs),fgcc(idiobs)


c
c     Trace-objects variables
c

      dimension xctra(idiobs),yctra(idiobs),dmalat(idiobs),
     ?dmilat(idiobs),agtra(idiobs),bcgt(idiobs),bcgst(idiobs)



c
c     FDP variables
c


      dimension adx(jfdp),ady(jfdp),coordx(jfdp),coordy(jfdp),cdx(2),
     ?cdy(2),xfdp(idiobs),yfdp(idiobs)



c
c     Vizier catalogue extraxtion variables
c


      dimension rack(idiobs),deck(idiobs)


c
c     2MASS catalogue variables
c

      dimension ra2ma(idi2ma),de2ma(idi2ma),dmgj(idi2ma),dmgh(idi2ma),
     ?dmgk(idi2ma),emgj(idi2ma),emgh(idi2ma),emgk(idi2ma),ddj2(idi2ma),
     ?id2ma(idiobs)




c
c     Ephemeris catalogue variables
c

      dimension raeph(idiobs),deeph(idiobs),eraeph(idiobs),
     ?edeeph(idiobs),pmrap(idiobs),pmdep(idiobs),epmrap(idiobs),
     ?epmdep(idiobs),udmgjp(idiobs),udmghp(idiobs),udmgkp(idiobs),
     ?udmgp(idiobs),cudmgp(idiobs),ideph(idiobs),xraeph(idiobs),
     ?ydeeph(idiobs),erap(idiobs),edep(idiobs),alfrep(idiobs),
     ?delrep(idiobs),coefxp(icofsp),coefyp(icofsp),ecofxp(icofsp),
     ?ecofyp(icofsp),itirap(idiobs),edmgjp(idiobs),edmghp(idiobs),
     ?edmgkp(idiobs),raucp(idiobs),deucp(idiobs)



c
c     Inverted (RA,Dec) reduction Ephemeris catalogue  variables
c

      dimension itirpi(idiobs),xraepi(idiobs),ydeepi(idiobs),
     ?erpi(idiobs),edpi(idiobs),alfrpi(idiobs),delrpi(idiobs),
     ?ciefxp(icofsp),ciefyp(icofsp),ecifxp(icofsp),ecifyp(icofsp)


c
c     User catalogue variables
c



      dimension raucs(idiobs),deucs(idiobs),eraucs(idiobs),
     ?edeucs(idiobs),pmras(idiobs),pmdes(idiobs),epmras(idiobs),
     ?epmdes(idiobs),udmgjs(idiobs),udmghs(idiobs),udmgks(idiobs),
     ?edmgjs(idiobs),edmghs(idiobs),edmgks(idiobs),udmgs(idiobs),
     ?cudmgs(idiobs),iducs(idiobs),itiras(idiobs),eras(idiobs),
     ?edes(idiobs),alfres(idiobs),delres(idiobs),coefxs(icofsp),
     ?coefys(icofsp),ecofxs(icofsp),ecofys(icofsp),xraucs(idiobs),
     ?ydeucs(idiobs),mfindu(idiobs)


c
c     Inverted (RA,Dec) reduction User catalogue  variables
c


      dimension xrauci(idiobs),ydeuci(idiobs),erai(idiobs),edei(idiobs),
     ?alfrei(idiobs),delrei(idiobs),coefxi(icofsp),coefyi(icofsp),
     ?ecofxi(icofsp),ecofyi(icofsp),itirai(idiobs)


c
c     Gaia DR3 variables
c


      dimension rag1(idigai),deg1(idigai),erag1(idigai),
     ?edeg1(idigai),pmag1(idigai),pmdg1(idigai),epmag1(idigai),
     ?epmdg1(idigai),g1mgj(idiobs),g1mgh(idiobs),g1mgk(idiobs),
     ?g1emgj(idiobs),g1emgh(idiobs),g1emgk(idiobs),ug1mgg(idiobs),
     ?cg1mgg(idigai),idga1(idiobs),itirg1(idiobs),erasg1(idiobs),
     ?edesg1(idiobs),alfrg1(idiobs),delrg1(idiobs),g1cofx(icofsp),
     ?g1cofy(icofsp),g1ecfx(icofsp),g1ecfy(icofsp),xrag1(idiobs),
     ?ydeg1(idiobs),plg1(idigai),eplg1(idigai),rvg1(idigai),
     ?mtira(idigai)

      dimension xold(idigai),yold(idigai),iior(idigai),jjor(idigai)


c
c     Inverted (RA,Dec) reduction Gaia DR3 variables
c


      dimension xragi(idigai),ydegi(idigai),erasgi(idigai),
     ?edesgi(idigai),alfrgi(idiobs),delrgi(idiobs),gicofx(icofsp),
     ?gicofy(icofsp),giecfx(icofsp),giecfy(icofsp),itirgi(idiobs)


c
c     Target variables
c

      dimension tara(idiobs),tade(idiobs),tadist(idiobs),tamag(idiobs),
     ?tafase(idiobs),tarad(idiobs),tawl(idiobs)


c
c     Post-detection and final (RA,Dec) reduction variables
c

      dimension coefam(icofsp),coefxe(icofsp),coefye(icofsp),xf(idigai),
     ?yf(idigai),idfg(idigai),idfu(idiobs),idfe(idiobs),idft(idiobs),
     ?fmag(idigai),coefsm(icofsp)


c
c    Remake astrometry variables
c


      dimension cmgu(idiobs),omag(idiobs),xra(idiobs),yde(idiobs)




c
c     File names variables
c


      character*(idin) infits,imfits,names(idin2),knfits(idin2),kbadpb,
     ?kbadpc,kbadpf,imes,ires,inxy

      character*(idin+10) ibadpb,ibadpc,ibadpf

      character*(linxy) ixyg(idiobs),ixyu(idiobs),ixyp(idiobs),mxy

      character*200 iredep,iredus,iredg1


      character*(idin50) centro,ialvos,ialvep,ialvus,ialvg1,nalvep,
     ?nalvus,nalvg1,malvep,malvus,malvg1,lredep,lredus,lredg1,lds9ep,
     ?lds9us,lds9g1,ids9ep,ids9us,ids9g1


c
c     NIMA,MPC output format variables
c

      character*1  obtipo
      character*1  band
      character*3  iau
      character*1  icat,icato

      character*1 isig
      character*27 ird



      character*(idin50) fotrel,redrep,redrus,redrg1,ifdp

      character*(idin50) cpraia,epraia

      character*(idin50) subf

      character*1  menos,iver,nome(idin50),ibrac,obtype(idiobs)
      character*69 ichobj,ihname(idin2),mchobj
      character*20 ichfil,iobalv(idiobs)

      character*200 linha

      character*8 kadx(jjfdp),kady(jjfdp),kcrpx,kcrpy,kcdx(2),kcdy(2)

      character*56 lodpsf(5)


      character*21 fcat2m(idiobs),fcatga(idiobs),rmcat2(idiobs),
     ?rmcatg(idiobs)

      character*24 erase

      character*5 kind(idiobs)


      data ibrac/' '/
      data menos/'-'/

      hmsgms(i,j,a)=i+j/60.d0+a/3600.d0

      dexy(xx,yy,zz,ww)=dsin(yy)*dsin(ww)+dcos(yy)*dcos(ww)*dcos(xx-zz)
      xpad(xx,yy,zz)=dcos(yy)*dsin(xx-zz)
      ypad(xx,yy,zz,ww)=dsin(yy)*dcos(ww)-dcos(yy)*dsin(ww)*dcos(xx-zz)

      alff (xx,yy,zz,ww)=zz+datan2(xx,dcos(ww)-yy*dsin(ww))
      deltt(xx,yy,zz,ww)=datan2((yy*dcos(ww)+dsin(ww))*dcos(xx-zz),
     ?dcos(ww)-yy*dsin(ww))


      snrbg(fobj,gain,apixel,nbg,sbg)=dsqrt(fobj/(1.d0/gain+(apixel*
     ?sbg**2)*(1.d0+1.d0/nbg)/fobj))


c
c     Initial data
c


      pi=0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c


      dj2000=2451545.d0

      epog1=2016.0d0

      boxviz=1.d0
      boxviz=boxviz/60.d0
      boxviz=boxviz**2

      tt0=2.8d-3

      izero=0

      zero=0.d0

      d99=99.999d0

      ireflex=+1



c
c     Default 2dg x 2dg sizes for the Catalogue FOV for
c     the extraction of Gaia and 2MASS stars.
c


      scola=1.d0

      scagai=-1.d0
      scausr=-1.d0
      scaeph=-1.d0
 
      fovx=2.d0
      fovy=2.d0
 
      expfav=1.d0

      nnnnnx=idnint(fovx*3600.d0)
      nnnnny=idnint(fovx*3600.d0)

      carx=grarad*fovx/2.d0
      cary=grarad*fovy/2.d0


c     ferpgm=2.d0*dsqrt(-2.d0*dlog(0.5d0))


c     ng(1)=1
      ng(1)=4
      ng(2)=5
      ng(3)=7
      ng(4)=7
      ng(5)=9


      qua=0.5d0
 
      gwi=2.d0

      flor=0.d0


c
c     Maximum allowed aperture size due to dimension limits on
c     variables (ida)
c


      gramal=idiobs
      gramal=gramal/pi
      gramal=dsqrt(gramal)
      gramal=gramal-2.d0

c

      um=1.d0

c

      do i=1,idiobs
      contag(i)=0.d0
      enddo

      do i=1,idiobs
      ior(i)=0
      enddo



c
c     Zeroing imagem, pixmat and matpix (image matrices' variables)
c

      do i=1,idimy
      do j=1,idimx
      pixmat(j,i)=0.
      enddo
      enddo

c
c     Defines the correct number of backspaces to safely apply a 1-line backspace
c     in an open file by FORTRAN.
c


      call backsp (1,nbac,91)

   

c
c     Reads input dada
c


c
c     Minimum, maximum magnitude range
c


      read (*,*) aux1
      read (*,*) aux2

      g2mag2=dmax1(aux1,aux2)
      g2mag1=dmin1(aux1,aux2)


c
c     - kpm:  key to cut Gaia DR3 stars without proper motions
c     - kpl:  key to cut Gaia DR3 stars without parallax
c     - kdup: key to cut Gaia DR3 stars with dupliticy
c

      read (*,*) kpm
      read (*,*) kpl
      read (*,*) kdup


c
c     User catalogue
c

      read (*,3) cpraia


c
c     Ephemeris catalogue
c

      read (*,3) epraia


c
c     Do (RA,Dec) reduction with User catalogue?
c

      read (*,*) iuserc


c
c     Do (RA,Dec) reduction with Ephemeris catalogue?
c

      read (*,*) iephec




c
c     Checks if a User reference catalogue is present.
c
c     If it is not present, overrides any user option and sets
c     iuserc=2 (no reduction)
c
c

      kuserc=2

      open (77,file=cpraia,status='old',err=705)

      read (77,702,err=705,end=705) iah,iam,sa,isig,idg,idm,sd,ex,ey,dj,
     ?pma,pmd,epma,epmd,cudmg,cudmg2

 702  format(i2,1x,i2,1x,f7.4,2x,a1,i2,1x,i2,1x,f6.3,2x,2f7.3,2x,f16.8,
     ?1x,4(1x,f7.3),2(1x,f6.3))


      kuserc=1


 705  close (77)


      if (kuserc.eq.2) then

      if (iuserc.eq.1) then

      write (*,*) 
      write (*,*) 
      write (*,*) 'Warning: User catalogue is not present. No (RA,Dec)'
      write (*,*) 'reductions with this catalogue can be done. Exiting.'
      write (*,*)
      write (*,*) 

      stop

      endif


      endif



c
c     Checks if an Ephemeris reference catalogue is present.
c
c     If it is not present, overrides any user option and sets
c     iephec=2 (no reduction)
c
c

      kephec=2

      open (77,file=epraia,status='old',err=706)

      read (77,703,err=706,end=706) iah,iam,sa,isig,idg,idm,sd,ex,ey,dj,
     ?pma,pmd,epma,epmd,cudmg,cudmg2

 703  format(i2,1x,i2,1x,f7.4,2x,a1,i2,1x,i2,1x,f6.3,2x,2f7.3,2x,f16.8,
     ?1x,4(1x,f7.3),2(1x,f6.3))


      kephec=1


 706  close (77)


      if (kephec.eq.2) then

      if (iephec.eq.1) then

      write (*,*) 
      write (*,*) 
      write (*,*) 'Warning: Ephemeris catalogue is not present. No'
      write (*,*) '(RA,Dec) reductions with this catalogue can be done.'
      write (*,*) 'Exiting.'
      write (*,*) 

      stop

      endif


      endif


c


      read (*,*) iastro

      read (*,3) centro

      read (*,3) ialvos

      read (*,*) mfdp

      read (*,3) ifdp
      read (*,*) kfdp

      read (*,*) lfdp

  
      read (*,3) kbadpb
      read (*,3) kbadpc
      read (*,3) kbadpf

      read (*,3) fotrel

      read (*,3) redrep
      read (*,3) redrg1
      read (*,3) redrus


      read (*,3) ialvep
      read (*,3) ialvg1
      read (*,3) ialvus


      read (*,3) malvep
      read (*,3) malvg1
      read (*,3) malvus


      read (*,3) nalvep
      read (*,3) nalvg1
      read (*,3) nalvus


      read (*,'(a1)') obtipo
      read (*,'(a1)') band
      read (*,'(a3)') iau
      read (*,'(a1)') icat



      read (*,3) lredep
      read (*,3) lredg1
      read (*,3) lredus

      read (*,3) lds9ep
      read (*,3) lds9g1
      read (*,3) lds9us


      read (*,*) tbox

      read (*,*) box

c     read (*,*) scala
c     read (*,*) ecala



      read (*,*) ipflag
      read (*,*) bscale
      read (*,*) bzero
      read (*,*) bitpyx
      read (*,*) kswap

      read (*,*) nswap

      read (*,*) gain

      read (*,*) modpsf

      lodpsf(1)='PRAIA Photogravity Center Method (PGM)'
      lodpsf(2)='2D Circular Gaussian'
      lodpsf(3)='2D Elliptical Gaussian'
      lodpsf(4)='2D Circular Cauchy-Lorentz with alpha & beta exponents'
      lodpsf(5)='2D Elliptical Cauchy-Lorentz with alpha & beta exponent
     ?s'


c     read (*,*) ifittr
      
      ifittr=2

      read (*,*) elimt


      lpert=4

c     read (*,*) iwr
      
      iwr=2

c     read (*,*) fapiro
c     read (*,*) dlimro
c     read (*,*) plimro
c     read (*,*)


      fapiro=4.d0

      dlimro=0.01d0

      plimro=0.001d0

      icorox=30

      fotceu=13.d0

c     read (*,*) iwt
      
      iwt=2


      fapitr=3.d0

      dlimtr=0.01d0

      plimtr=0.001d0

      icotr=30

c     read (*,*) icotr

      icotr=30

      ifindg=1
      ifindu=1
      ifinde=1
      ifindt=1
      efind=0.d0


      if (iuserc.ne.1) ifindu=2

      if (iephec.ne.1) ifinde=2



      nbcat=60
      nbmed=15



c
c     Catalogue FOV input by the user, as an alternative to
c     the default 2dg x 2dg sizes
c

      read (*,*) barx
      read (*,*) bary


      if (barx.gt.0.d0.and.bary.gt.0.d0) then
 
      nnnnnx=idnint(barx*60.d0)
      nnnnny=idnint(bary*60.d0)
 
      carx=grarad*(barx/60.d0)/2.d0
      cary=grarad*(bary/60.d0)/2.d0

      endif


c     write (*,*)
c     write (*,*) 'nnnnnx = ',nnnnnx
c     write (*,*) 'nnnnny = ',nnnnny
c     write (*,*) 'carx   = ',carx*radgra*60
c     write (*,*) 'cary   = ',cary*radgra*60
c     write (*,*)
c     write (*,*)
c     stop



c

      read (*,*) erpix

      read (*,*) iw

      read (*,*) pcorep
      read (*,*) pcorg1
      read (*,*) pcorus

      read (*,*) index

      read (*,*) inicio
      read (*,*) iultmo


c     read (*,*) itx
c     read (*,*) ity
c

      itx=0635
      ity=0530

c


 3    format(a50)


c     close (1)



c     erpix=erpix/scala



c
c
c     write (*,*) ' '
c     write (*,*) ' '
c     write (*,*) ' '
c     write (*,1)
c1    format (23x,'PRAIA - astrometric and photometric setup')
c
c     write (*,*) ' '
c     write (*,*) ' '
c     write (*,*) ' '
 

c
 
c     rewind (5)


c2    continue


c     read (*,5) linha
 5    format(a200)

c     write (*,5) linha

c     if (linha(1:1).ne.'*') go to 2


      write (*,*) ' '
      write (*,*) ' '
      write (*,*) ' '



c
c     Organizing the fields to be treated
c


      open (1,file=centro)


      i=0
 4    read (1,*,end=2028)
      i=i+1
      go to 4

 2028 close (1)

      iul=i

      if (iul.eq.0) then
      write (*,*) ' '
      write (*,*) 'No field data furnished. Exiting program.'
      write (*,*) ' '
      stop
      endif
      

c
      
      if (inicio.eq.0 .and. iultmo.eq.0) then

      inicio=1
      iultmo=iul


      else

      if (inicio.gt.iultmo) then
      write (*,*) ' '
      write (*,*) 'Initial/Final field data file ranks do not match. Exi
     ?ting program.'
      write (*,*) ' '
      stop
      endif


      if (inicio.gt.iul .or. inicio.lt.1) then
      write (*,*) ' '
      write (*,*) 'Initial field data file outside list range. Exiting p
     ?rogram.'
      write (*,*) ' '
      stop
      endif

      if (iultmo.gt.iul .or. iultmo.lt.1) then
      write (*,*) ' '
      write (*,*) 'Final field data file outside list range. Exiting pro
     ?gram.'
      write (*,*) ' '
      stop
      endif

      endif




c
c     Stores file names and object names
c


      open (3,file=centro)


      do i=1,inicio-1
      read(3,*,end=200)
      enddo

      do 190 i=inicio,iultmo

      names(i)=''
      ihname(i)=''


      if (iastro.eq.0) then

      read(3,402,err=194,end=200) iah,iam,sa,isig,idg,idm,ds,iuth,
     ?iutm,sut,iutano,iutmes,iutdia,djm,dj,iexps,ichfil,names(i),mchobj

 402  format(1x,i2,1x,i2,1x,f7.4,1x,a1,i2,1x,i2,1x,f6.3,2x,i2,1x,i2,
     ?1x,f5.2,1x,i4,1x,i2,1x,i2,f16.8,1x,f16.8,2x,i4,2x,a20,2x,a50,
     ?1x,a20)


      ihname(i)=mchobj


      else


      read(3,91,err=194,end=200) names(i)

      open(92,file=names(i))

      read (92,470,err=196,end=196) xob(i),yob(i),seng(i),altu(i),
     ?fgcc(i),fumag,fumag2,xmgu,cudmg,cudmg2,xmgj,xmgh,xmgk,res2mg,
     ?resmg2,ermgj,ermgh,ermgk,pma,pmd,epma,epmd,ex,ey,erau,edeu,alfsiu,
     ?delsiu,nstaru,nfinau,alsiu,desiu,ktirau,ra,de,iuth,iutm,sut,
     ?iutano,iutmes,iutdia,dj,iexps,ichfil,imfits,mchobj,nx,ny

 470  format(2(1x,f7.2),1x,f5.2,2(f10.2),13(1x,f6.3),4(1x,f7.3),
     ?6(1x,f6.3),2(1x,i4),2(1x,f6.3),1x,i4,2(1x,f13.9),1x,i2,1x,i2,
     ?1x,f5.2,1x,i4,1x,i2,1x,i2,1x,f16.8,2x,i4,2x,a20,2x,a50,1x,a20,
     ?2(1x,i5))


      ihname(i)=mchobj

      knfits(i)=imfits

      close (92)


      endif



 190  continue


      go to 200


c

 194  write (*,195) names(i)
 195  format(1x,'Reading error on field data file list: ',a200)
      write (*,*)
      write (*,*)'Exiting program.'

      close (3)

      stop

c

 196  write (*,195) names(i)
      write (*,*)
      write (*,*)'Exiting program.'

      close (92)
      close (3)

      stop

c

 200  continue


      close (3)




c
c     Super loop of all-fields astrometry
c


      kat=0
      katrm=0


      do 60 lllll=inicio,iultmo


c


      iredep=''
      iredus=''
      iredg1=''


      infits=''
      infits=names(lllll)


c
c     Mounts names of input and output field files
c


      if (iastro.ne.0) infits=knfits(lllll)


      do ii=1,idin
      if (infits(ii:ii+5).eq.'.fits ') go to 2000
      if (infits(ii:ii+5).eq.'.FITS ') go to 2000
      if (infits(ii:ii+4).eq.'.fts ') go to 2000
      if (infits(ii:ii+4).eq.'.FTS ') go to 2000
      if (infits(ii:ii+4).eq.'.fit ') go to 2000
      if (infits(ii:ii+4).eq.'.FIT ') go to 2000
      if (infits(ii:ii).eq.ibrac) go to 2000
      enddo



c


 2000 continue

      ii=ii-1

      kkii=ii

c

      iredep=''
      iredus=''
      iredg1=''

      iredep(1:ii)=infits(1:ii)
      iredus(1:ii)=infits(1:ii)
      iredg1(1:ii)=infits(1:ii)


      iredep(ii+1:ii+1)='.'
      iredus(ii+1:ii+1)='.'
      iredg1(ii+1:ii+1)='.'

      ids9ep=''
      ids9us=''
      ids9g1=''


      ids9ep=iredep
      ids9us=iredus
      ids9g1=iredg1



c
c     Gaia DR3 xy file
c

      do iii=idin50,1,-1
      if (lredg1(iii:iii).ne.ibrac) go to 2001
      enddo

 2001 continue

      iredg1(ii+2:ii+1+iii)=lredg1



      
c
c     Ephemeris catalogue xy file
c

      do iii=idin50,1,-1
      if (lredep(iii:iii).ne.ibrac) go to 2002
      enddo

 2002 continue

      iredep(ii+2:ii+1+iii)=lredep


      
c
c     User reference catalog xy file
c

      do iii=idin50,1,-1
      if (lredus(iii:iii).ne.ibrac) go to 2003
      enddo

 2003 continue

      iredus(ii+2:ii+1+iii)=lredus



c
c     Gaia DR3 ds9 file
c

      do iii=idin50,1,-1
      if (lds9g1(iii:iii).ne.ibrac) go to 2004
      enddo

 2004 continue

      ids9g1(ii+2:ii+1+iii)=lds9g1



      
c
c     Ephemeris catalogue ds9 file
c

      do iii=idin50,1,-1
      if (lds9ep(iii:iii).ne.ibrac) go to 2005
      enddo

 2005 continue

      ids9ep(ii+2:ii+1+iii)=lds9ep


      
c
c     User reference catalog ds9 file
c

      do iii=idin50,1,-1
      if (lds9us(iii:iii).ne.ibrac) go to 2006
      enddo

 2006 continue

      ids9us(ii+2:ii+1+iii)=lds9us



c     

 91   format(a150)
 11   format(a50)
 12   format(50a1)
 92   format(150a1)
 93   format(a200)



c
c     Bad pixels file name
c

      ibadpb=''
      ibadpc=''
      ibadpf=''

      ibadpb(1:kkii)=infits(1:kkii)
      ibadpc(1:kkii)=infits(1:kkii)
      ibadpf(1:kkii)=infits(1:kkii)


      ibadpb(kkii+1:kkii+5)='.bbpx'
      ibadpc(kkii+1:kkii+5)='.bcpx'
      ibadpf(kkii+1:kkii+5)='.bfpx'




c
c     File names of objects and targets (round and trace-shaped)
c     in ds9 format input by the user for (x,y) measurements
c

      imes=''
      imes(1:kkii)=infits(1:kkii)
      imes(kkii+1:kkii+4)='.mes'
      ires=''
      ires=imes
      ires(kkii+1:kkii+4)='.res'



c
c     write (*,91) infits
c     write (*,93) iredep
c     write (*,93) iredg1
c     write (*,93) iredus
c     write (*,91) ids9
c     write (*,91) ibadpx
c     stop
c



c

      write (*,*)
      write (*,*)


      if (iastro.eq.0) then

      write (*,14) lllll,iultmo,infits
 14   format (1x,'Proccessing field ',i5,' of ',i5,': file = ',a150)

      else

      infits=names(lllll)

      write (*,14) lllll,iultmo,infits

      endif


      write (*,*)


c
c     Field Distortion Pattern. Corrects field distortions by applying
c     (dx,dy) offsets for the (x,y) measurements, or (dRA,dDec) offsets
c     for the (RA,Dec)s (remaking astrometry mode 3 tangent plane technique).
c
 
      nfdp=0

c


      kcrpx=''
      kcrpy=''


      do i=1,2
      kcdx(i)=''
      kcdy(i)=''
      cdx(i)=0.d0
      cdy(i)=0.d0
      enddo


      do i=1,jjfdp
      kadx(i)=''
      kady(i)=''
      enddo


      do i=1,jfdp
      adx(i)=0.d0
      ady(i)=0.d0
      coordx(i)=0.d0
      coordy(i)=0.d0
      enddo


      do i=1,idiobs
      xfdp(i)=0.d0
      yfdp(i)=0.d0
      enddo

c

      open (23,file=ifdp,status='old',err=280)


c
c     FDP mode 1.
c
c     Gives the mask with (dx,dy) in pixels as a function of (x,y)s or
c     (dRA,dDec) in arcseconds as a function of (RA,Dec)
c


      if (mfdp.eq.1) then

      do i=1,jfdp
      read (23,*,end=275) adx(i),ady(i),coordx(i),coordy(i)
      enddo

      endif


c
c     FDP mode 2.
c
c     Gives directly the coefficients in (x,y) in pixels or (RA,Dec) in
c     arcseconds describing the 3rd degree bivariate distortion polynomial model.
c
c     Here the 3rd degree distortion model follows the astrometric convention:
c
c
c     Pol(x)=a1+a2x+a3y+a4x**2+a5x*y+a6y**2+a7x**3+a8x**2y+a9x*y**2+a10y**3
c
c     Pol(y)=b1+b2x+b3y+b4x**2+b5x*y+b6y**2+b7x**3+b8x**2y+b9x*y**2+b10y**3
c



      if (mfdp.eq.2) then

      do i=1,jjfdp
      read (23,*,end=275) adx(i),ady(i)
      enddo

      endif


c
c
c     FDP mode 3. The 3rd degree bivariate distortion polynomial model coefficients
c     are furnished through WCS FITS header keys.
c
c     Here the 3rd degree distortion model do not follow the astrometric convention.
c     It follows the WCS FITS header convention:
c
c
c     Pol(x)=a1+a2x+a3y+a4x**2+a5x*y+a6y**2+a7x**3+a8x**2y+a9x*y**2+a10y**3
c
c     Pol(y)=b1+b2y+b3x+b4y**2+b5y*x+b6x**2+b7y**3+b8y**2x+b9y*x**2+b10x**3
c
c
c     It is assumed that Pol describes distortions as a function of (x,y) in pixels.
c
c

      if (mfdp.eq.3) then

      do i=1,jjfdp
      read (23,*,end=275) kadx(i),kady(i)
      enddo

      endif


c
c
c     FDP mode 4. DES DECAM procedure. 
c
c     The 3rd degree bivariate distortion polynomial model coefficients
c     are furnished through WCS FITS header keys. As in mode 3, it does not follow
c     the astrometric convention. It follows the WCS FITS header convention.
c
c     Extra coefficiennts are furnished for characterizing the coordinates of
c     the central pixel and scale and rotation of axex, prior for applying the
c     distortion polynomial corrections.
c
c

      if (mfdp.eq.4) then

      do i=1,jjfdp
      read (23,*,end=275) kadx(i),kady(i)
      enddo

      read (23,*,end=275) kcrpx,kcrpy


      do j=1,2
      read (23,*,end=275) kcdx(j),kcdy(j)
      enddo


      endif

c


 275  nfdp=i-1

 280  close (23)



c
c     Astrometry of fits images
c


      if (iastro.ne.0) go to 150



c
c     Reads fits image and stores it into a (nx,ny) matrix.
c

      bitpix=bitpyx

      write (*,*)

      call refits (idimx,idimy,pixmat,infits,nx,ny,nheads,ichobj,ipflag,
     ?bscale,bzero,kswap,iswap,nswap,bitpix,mfdp,jfdp,jjfdp,nfdp,adx,
     ?ady,kadx,kady,crpx,crpy,cdx,cdy,kcrpx,kcrpy,kcdx,kcdy)

      ichobj=ihname(lllll)


      write (*,*)


c
c
c     Masks of bad pixels input by the user to be excluded of any
c     image processing. Excluded pixels are marked by setting
c     negative ADU counts for them in the pixel matrix.
c
c
c     For all valid pixels (independent of the presence or not of
c     valid negative pixel counts), a fixed (positive or negative)
c     value is added to all the valid pixels in the image, so that
c     the mimimum pixel count in the image is set to 100 ADU counts.
c
c
c     The pixels to be excluded are setup from pixel masks
c     furnished by the user. There are 3 types of pixel masks:
c
c        1 - rectangular areas
c
c        2 - circular areas
c
c        3 - Range of ADU pixel counts
c
c
c
c     There are masks valid for all images, and masks for
c     individual images.
c
c
c     Masks for all images:
c
c     kbadpf - bad pixel mask file with bad range of ADU counts
c
c     kbadpb - bad pixel mask file with bad rectangular regions
c
c     kbadpc - bad pixel mask file with bad regions
c
c
c     Masks for individual images:
c
c     ibadpf - bad pixel mask file with bad range of ADU counts
c
c     ibadpb - bad pixel mask file with bad rectangular regions
c
c     ibadpc - bad pixel mask file with bad regions
c
c
c
c         NaN pixel values are also automatically masked.
c
c
c



      call pimask (idin,idimx,idimy,pixmat,imagem,nx,ny,kbadpf,kbadpb,
     ?kbadpc,ibadpf,ibadpb,ibadpc)



c
c     Initiates object identification and measuring
c


      write (*,*)
      write (*,*) 'Identifing and measuring objects.'
      write (*,*)
      write (*,*)



c
c     Stores pixel region data (*.mes files) for targets directly furnished by
c     the user. The region data conforms to the ds9 region format.
c
c     Circular regions are associated to Gaussian PSFs.
c
c     Rectangular regions are associated to trace-shaped PSFs
c     (Error-Function-based PSF).
c


      call mesure (idin,idiobs,imes,nmcir,xcir,ycir,lacir,bcgc,bcgsc,
     ?nmtra,xtra,ytra,xlatra,ylatra,angtra,bcgtr,bcgstr)




c
c     Excludes from the identification process the pixels of the circular
c     regions of rounded-shaped targets directly furnished by the user.
c



c     do k=1,nmcir
c
c     ix1=xcir(k)-lacir(k)
c     ix2=xcir(k)+lacir(k)
c     iy1=ycir(k)-lacir(k)
c     iy2=ycir(k)+lacir(k)
c
c     if (ix1.lt.1) ix1=1
c     if (iy1.lt.1) iy1=1
c     if (ix2.gt.nx) ix2=nx
c     if (iy2.gt.ny) iy2=ny
c
c     raio=lacir(k)
c     xc=xcir(k)
c     yc=ycir(k)
c
c     do i=iy1,iy2
c     do j=ix1,ix2
c
c     call circul(raio,xc,yc,j,i,ichave)
c
c     if (ichave.gt.0) imagem(j,i)=-1 
c
c     enddo
c     enddo
c
c     enddo



c
c     Excludes from the identification process the pixels of the rectangular
c     regions of trace-shaped targets directly furnished by the user.
c




c     do k=1,nmtra
c
c
c     ix1=xtra(k)-xlatra(k)
c     ix2=xtra(k)+xlatra(k)
c     iy1=ytra(k)-ylatra(k)
c     iy2=ytra(k)+ylatra(k)
c
c     cs=dcos(grarad*angtra(k))
c     sn=dsin(grarad*angtra(k))
c
c     do     i=iy1,iy2
c     do 272 j=ix1,ix2
c
c     ix=+(j-xtra(k))*cs+(y-ytra(k))*sn
c     iy=-(j-xtra(k))*sn+(y-ytra(k))*cs
c
c
c     if (ix.lt.1)  go to 272
c     if (iy.lt.1)  go to 272
c     if (ix.gt.nx) go to 272
c     if (iy.gt.ny) go to 272
c
c     imagem(ix,iy)=-1 
c
c
c272  continue
c
c     enddo
c
c     enddo




c
c
c     Identifies candidate objects with PRAIA's BOIA method.
c
c     Candidate objects are detected and classified as round-shaped objects or not.
c     If not, they are stored as regions potentially containing part or all of a
c     candidate trace-shaped object.
c
c     Parameters defining the regions' and candidate objects' characteristics are
c     stored for the fitting proccess of rounded-shaped objects, and for the analysis
c     of the regions with candidate traced-shaped objects.
c
c


      call ident (idimx,idimy,idiobs,icofsp,coefx,pixmat,imagem,nx,ny,
     ?ior,xid,yid,idx1,idx2,idy1,idy2,idlado,npix,bcg,bcgs,nbcg,dan,
     ?exce,sige,snr,cf,cra,cann,uu20,uu02,uu11,seg1,seg2,nstar,snrmin,
     ?gain,sv,ixb,iyb,qua,gramal,marca,mx,my,ncells,iddo)



c
c
c     Analyses regions with candidate trace-shaped objects identified with "ident".
c
c     Identifies trace-shaped objects.
c
c     Removes rounded-shaped candidates from the initial list that turned out to be
c     part of a trace-shaped object. 
c    
c
c

c     do k=1,nstar
c     write(805,"('circle(',2(f8.2,','),f8.2,')')") xid(k),yid(k),
c    ?cra(k)
c     write(806,"('circle(',2(f8.2,','),f8.2,')')") xid(k),yid(k),
c    ?cann(k)
c55   format('circle(',2(f8.2,','),f8.2,')')
c     write (802,*) exce(k),seg1(k),seg2(k),dan(k),xid(k),yid(k)
c     enddo


      write (*,*)
      write (*,*)
      write (*,*) 'Separates rounded-shaped objects from trace-shaped (i
     ?f existent) ...'
      write (*,*)


c
c     Here the trace routine is temporarily suspended. We simply
c     identify and eliminate objects with excentricity > E, with
c     the E threshold set by the user. 
c


      m=nstar

      nstar=0

      do k=1,m

      if (exce(k).lt.elimt) then

      nstar=nstar+1

      xid(nstar)=xid(k)
      yid(nstar)=yid(k)
      idlado(nstar)=idlado(k)

      idx1(nstar)=idx1(k)
      idx2(nstar)=idx2(k)
      idy1(nstar)=idy1(k)
      idy2(nstar)=idy2(k)

      npix(nstar)=npix(k)

      bcg(nstar)=bcg(k)
      bcgs(nstar)=bcgs(k)
      nbcg(nstar)=nbcg(k)

      dan(nstar)=dan(k)
      exce(nstar)=exce(k)
      sige(nstar)=sige(k)
      seg1(nstar)=seg1(k)
      seg2(nstar)=seg2(k)
      snr(nstar)=snr(k)
      cf(nstar)=cf(k)
      cra(nstar)=cra(k)
      cann(nstar)=cann(k)
      uu20(nstar)=uu20(k)
      uu02(nstar)=uu02(k)
      uu11(nstar)=uu11(k)

      endif

      enddo




c     call idtrack (idimx,idimy,idiobs,icofsp,coefx,xest,yest,xp,pixmat,
c    ?imagem,nx,ny,contag,fotceu,lpert,iflag,ior,nval,xid,yid,idx1,idx2,
c    ?idy1,idy2,idlado,npix,bcg,bcgs,nbcg,dan,exce,sige,snr,cf,cra,cann,
c    ?uu20,uu02,uu11,seg1,seg2,nstar,elimt,xctra,yctra,dmalat,dmilat,
c    ?agtra,bcgt,bcgst,ntrac,u,v,w)

      ntrac=0

      write (*,*)
      write (*,*) 'Separation of rounded/trace shaped objects finished.'
      write (*,*)


c
c     Initiates the (x,y) measurements
c


      write (*,*)
      write (*,*)
      write (*,*) '(x,y) measurements. Round-shaped objets: ',
     ?lodpsf(modpsf)
      write (*,*)



c
c     Determines the (x,y) center of primary-detected rounded-shaped
c     objects automatically identified by PRAIA BOIA method, using
c     one of the available options:
c
c
c      - key 1: PRAIA's PhotoGravicenter Method (PGM)
c
c      - key 2: 2D Circular Gaussian model
c
c      - key 3: 2D Elliptical Gaussian model
c
c      - key 4: 2D Circular Cauchy-Lorentz model
c
c      - key 5: 2D Elliptical Cauchy-Lorentz model
c
c
c
c



      nest=0

      ierro=0

      do 57 k=1,nstar

      if (npix(k).lt.ng(modpsf)) go to 57

c

      xc=xid(k)
      yc=yid(k)

      ix1=idx1(k)
      ix2=idx2(k)
      iy1=idy1(k)
      iy2=idy2(k)

      fc=bcg(k)

      fs=bcgs(k)
      nfs=nbcg(k)

      pc=cf(k)

      sig=sige(k)

      eg1=seg1(k)
      eg2=seg2(k)

      u20=uu20(k)
      u02=uu02(k)
      u11=uu11(k)

      tet=dan(k)

      r=cra(k)


c     write(800,"('circle(',2(f8.2,','),f8.2,')')") xc,yc,r


c
c     The 2D PSF fit
c

      IF (modpsf.ne.1) THEN


c     xx=1709.38d0
c     yy=1556.19d0
c
c     dd=dsqrt((xc-xx)**2+(yc-yy)**2)
c
c     IF (dd.lt.-1.0d0) THEN
c
c     write (*,*)
c     write (*,*)
c     write (*,*) 'raio BOIA = ',r
c     write (*,*) 'x,y       = ',xc,yc
c     write (*,*)
c     write (*,*)
c
c     c1=2.d0
c     c2=cann(k)
c
c     pass=0.1d0
c
c     r=c1-pass
c
c
c7770 continue
c
c     r=r+pass
c
c     if (r.gt.c2) go to 7799
c
c
c     xc=xid(k)
c     yc=yid(k)
c
c     fc=bcg(k)
c
c     fs=bcgs(k)
c     nfs=nbcg(k)
c
c     sig=sige(k)
c
c     eg1=seg1(k)
c     eg2=seg2(k)
c
c     u20=uu20(k)
c     u02=uu02(k)
c     u11=uu11(k)
c
c     tet=dan(k)
c
c
c     call flux (idimx,idimy,pixmat,nx,ny,xc,yc,r,soma,a)
c
c     if (a.lt.ng(modpsf)) go to 7770
c
c     pc=soma-a*fc
c
c     snra=snrbg(dabs(pc),gain,a,nfs,fs)
c
c
c     call psf (idimx,idimy,idiobs,icofsp,pixmat,imagem,fapiro,dlimro,
c    ?plimro,icorox,nx,ny,ix1,ix2,iy1,iy2,r,xc,yc,ex,ey,sig,hh,fc,pc,fs,
c    ?nfs,u20,u02,u11,eg1,eg2,tet,exc,iwr,modpsf,ierro,resx,sv,ior,xest,
c    ?yest,xp,coefx,u,v,w,wsig,floor,qua)
c
c     if (ierro.eq.1) then
c     ierro=0
c     go to 7770
c     endif
c
c     fwhm=2.d0*sig*1.177410023d0
c
c     seeing=fwhm*scala
c
c     eg12=(r**2)/dsqrt(1.d0-exc**2)
c     eg22=dsqrt(1.d0-exc**2)*r**2
c     ex=dsqrt((dcos(tet)**2)*eg12+(dsin(tet)**2)*eg22)
c     ey=dsqrt((dcos(tet)**2)*eg22+(dsin(tet)**2)*eg12)
c     ex=1000.d0*scala*dsqrt(pi)*ex/snra
c     ey=1000.d0*scala*dsqrt(pi)*ey/snra
c
c     erro=dsqrt(ex**2+ey**2)
c
c     ratio=r/fwhm
c
c     write (1000,*) snra,r,erro,ex,ey,fwhm,seeing,ratio
c
c     go to 7770
c
c7799 continue
c
c     STOP
c
c     ENDIF



      call psf (idimx,idimy,idiobs,icofsp,pixmat,imagem,fapiro,dlimro,
     ?plimro,icorox,nx,ny,ix1,ix2,iy1,iy2,r,xc,yc,ex,ey,sig,hh,fc,pc,fs,
     ?nfs,u20,u02,u11,eg1,eg2,tet,exc,iwr,modpsf,ierro,resx,sv,ior,xest,
     ?yest,xp,coefx,u,v,w,wsig,floor,qua)

c     write (*,*) 'k,ierro,sig,r,xc,yc,resx,bcgs,ratio = ',k,ierro,sig,
c    ?r,xc,yc,resx,bcgs(k),resx/bcgs(k)
c
c     write (999,*) k,resx/bcgs(k),r/sig
c
c     write (77,77) xc,yc,r
c77   format('circle(',2(f8.2,','),f8.2,')')


      if (ierro.eq.1) then
      ierro=0
      go to 57
      endif

c     if (sig.gt.r) go to 57

      if (xc.lt.ix1-0.5d0) go to 57
      if (xc.gt.ix2+0.5d0) go to 57
      if (yc.lt.iy1-0.5d0) go to 57
      if (yc.gt.iy2+0.5d0) go to 57


      ENDIF



c
c     Stores object data
c

      nest=nest+1

      xid(nest)=xid(k)
      yid(nest)=yid(k)
      idx1(nest)=idx1(k)
      idx2(nest)=idx2(k)
      idy1(nest)=idy1(k)
      idy2(nest)=idy2(k)
      idlado(nest)=idlado(k)
      npix(nest)=npix(k)
      bcg(nest)=bcg(k)
      bcgs(nest)=bcgs(k)
      nbcg(nest)=nbcg(k)
      dan(nest)=dan(k)
      exce(nest)=exce(k)
      sige(nest)=sige(k)
      seg1(nest)=seg1(k)
      seg2(nest)=seg2(k)
      snr(nest)=snr(k)
      cf(nest)=cf(k)
      cra(nest)=cra(k)
      cann(nest)=cann(k)
      uu20(nest)=uu20(k)
      uu02(nest)=uu02(k)
      uu11(nest)=uu11(k)


      IF (modpsf.ne.1) THEN

c
c     2D PSF models
c

      fwhm=2.d0*sig*1.177410023d0

c     seeing=fwhm*scala
      seeing=fwhm


      xob(nest)=xc
      yob(nest)=yc
      ilado(nest)=r
      seng(nest)=seeing

      altu(nest)=hh
      ialtu(nest)=nest

      expix(nest)=ex
      eypix(nest)=ey
      sgcc(nest)=sig
      fgcc(nest)=fc

      angle(nest)=tet


      sigg=seng(nest)/(2.d0*1.177410023d0)
c     volum(nest)=2.d0*pi*altu(nest)*sigg**2

      volum(nest)=cf(nest)


      obtype(nest)='r'


      ELSE

c
c     PRAIA Photogravicenter Method (PGM)
c


      fwhm=2.d0*sig*1.177410023d0

c     seeing=fwhm*scala
      seeing=fwhm


      xob(nest)=xid(nest)
      yob(nest)=yid(nest)
      ilado(nest)=r
      seng(nest)=seeing

      ixc=xob(nest)
      iyc=yob(nest)

      altu(nest)=pixmat(ixc,iyc)-fc
      ialtu(nest)=nest


c     ex=npix(nest)
c
c     ex=dsqrt(ex)/snr(nest)
c     ey=ex
c
c
c
c
cc    ex=dsqrt((sig/dsqrt(pc))**2+8.d0*pi*(fc+fs**2)*sig**4/pc)
c
c
c     ex=sig/snr(nest)
c
c     ey=ex
c
c
c     ex=dsqrt((dcos(tet)**2)*eg1**2+(dsin(tet)**2)*eg2**2)/snr(nest)
c     ey=dsqrt((dcos(tet)**2)*eg2**2+(dsin(tet)**2)*eg1**2)/snr(nest)
c
c     ex=ex*ferpgm
c     ey=ey*ferpgm
c
c
c     ex=dsqrt((dcos(tet)**2)*eg1**2+(dsin(tet)**2)*eg2**2)
c     ey=dsqrt((dcos(tet)**2)*eg2**2+(dsin(tet)**2)*eg1**2)
c
c
c     eeb=1.d0/dsqrt(cf(nest))
c
cc    eef=2.d0*dsqrt(2.d0*pi*bcg(nest))/cf(nest)
c
cc    eef=2.d0*dsqrt(2.d0*pi*bcgs(nest))/cf(nest)
c
cc    eef=2.d0*dsqrt(2.d0*pi)*bcgs(nest)/cf(nest)
c
cc    eef=2.d0*dsqrt(2.d0*pi)/snr(nest)
c
cc    eef=2.d0*dsqrt(2.d0*pi*npix(nest))*bcgs(nest)/cf(nest)
c
c     eef=2.d0*dsqrt(2.d0*pi*nfs)*bcgs(nest)/cf(nest)
c
c
c     ex=ex*dsqrt(eeb**2+eef**2)
c     ey=ey*dsqrt(eeb**2+eef**2)
c
c
c     ex=ex/dsqrt(eg1*eg2)
c     ey=ey/dsqrt(eg1*eg2)
c
c     ex=ex*dsqrt(dble(npix(nest)))/snr(nest)
c     ey=ey*dsqrt(dble(npix(nest)))/snr(nest)
c
c
c     ex=dsqrt((dcos(tet)**2)*eg1**2+(dsin(tet)**2)*eg2**2)
c     ey=dsqrt((dcos(tet)**2)*eg2**2+(dsin(tet)**2)*eg1**2)
c     ex=ex*ferpgm/2.d0
c     ey=ey*ferpgm/2.d0
c     ex=ex/dsqrt(pi)
c     ey=ey/dsqrt(pi)
c     eeb=1.d0/dsqrt(cf(nest))
c     eef=2.d0*dsqrt(2.d0*pi*bcg(nest))/cf(nest)
c     ex=ex*dsqrt(eeb**2+eef**2)
c     ey=ey*dsqrt(eeb**2+eef**2)
c
c
c     nbip=1
c
c     ex=dsqrt((dcos(tet)**2)*eg1**2+(dsin(tet)**2)*eg2**2)
c     ey=dsqrt((dcos(tet)**2)*eg2**2+(dsin(tet)**2)*eg1**2)
c     eeb=1.d0/dsqrt(gain*cf(nest))
cc    eef=2.d0*dsqrt(2.d0*pi*gain*bcg(nest)*4.d0)/(gain*cf(nest))
cc    eef=2.d0*dsqrt(2.d0*pi*npix(nest)*4.d0)*bcgs(nest)/cf(nest)
c     eef=2.d0*dsqrt(2.d0*pi*pi*eg1*eg2*nbip)*bcgs(nest)/cf(nest)
c     ex=ex*dsqrt(eeb**2+eef**2)
c     ey=ey*dsqrt(eeb**2+eef**2)
c
c
c     ex=dsqrt((dcos(tet)**2)*eg1**2+(dsin(tet)**2)*eg2**2)
c     ey=dsqrt((dcos(tet)**2)*eg2**2+(dsin(tet)**2)*eg1**2)
c     ex=nbip*ex/snr(nest)
c     ey=nbip*ey/snr(nest)
c     ex=dsqrt(pi)*ex*ferpgm
c     ey=dsqrt(pi)*ey*ferpgm
c
c
c     ex=dsqrt(pi)*cra(nest)/snr(nest)
c     ey=dsqrt(pi)*cra(nest)/snr(nest)
c
c
c
c
c     eg12=(cra(nest)**2)/dsqrt(1.d0-(exce(nest))**2)
c     eg22=dsqrt(1.d0-(exce(nest))**2)*(cra(nest))**2
c     ex=dsqrt((dcos(tet)**2)*eg12+(dsin(tet)**2)*eg22)
c     ey=dsqrt((dcos(tet)**2)*eg22+(dsin(tet)**2)*eg12)
c     ex=dsqrt(pi)*ex/snr(nest)
c     ey=dsqrt(pi)*ey/snr(nest)
c
c
c     area=pi*cra(nest)**2
c
c
c
c     area=-2.d0*dlog(0.5d0)*pi*seg1(nest)*seg2(nest)
c
c     eg12=1.d0/dsqrt(1.d0-(exce(nest))**2)
c     eg22=dsqrt(1.d0-(exce(nest))**2)
c     ex=dsqrt((dcos(tet)**2)*eg12+(dsin(tet)**2)*eg22)
c     ey=dsqrt((dcos(tet)**2)*eg22+(dsin(tet)**2)*eg12)
c     ex=dsqrt(area)*ex/snr(nest)
c     ey=dsqrt(area)*ey/snr(nest)
c
c
cc    sss=-2.d0*dlog(0.5d0)*sige(nest)**2/cra(nest)
cc
cc    ccc=gain*cf(nest)
cc
cc    area=dsqrt((1.d0/(ccc)+8.d0*pi*bcg(nest)*(sss**2)
cc   ?/(ccc)**2)*sss**2)
c
c
c
c     sss=2.d0*dsqrt(-2.d0*dlog(0.5d0))*sige(nest)
c     sss=cra(nest)
c
c     sss=cra(nest)
c     bri=1.d0/snr(nest)
c     fai=0.d0
c
c
c
c     sss=2.d0*dsqrt(-2.d0*dlog(0.5d0))*sige(nest)
c     sss=dsqrt(pi)*cra(nest)
c     sss=2.d0*dsqrt(-2.d0*dlog(0.5d0))*sige(nest)/(cra(nest)/(2.d0*
c    ?dsqrt(-2.d0*dlog(0.5d0))*sige(nest)))
c     sss=2.5d0*sige(nest)/(cra(nest)/(2.5d0*sige(nest)))
c     sss=2.d0*dsqrt(-2.d0*dlog(0.5d0))*sige(nest)
c     sss=sige(nest)
c     sss=2.5d0*sige(nest)/(cra(nest)/(2.5d0*sige(nest)))
c     sss=cra(nest)
c     sss=dsqrt(pi)*cra(nest)
c     sss=sige(nest)
c
c
c
c     hh=pixmat(idnint(xob(nest)),idnint(yob(nest)))-bcg(nest)
c
c
c     ebg=4.d0*pi*(sige(nest)**2)/(cf(nest)**2)
c     ebg=4.d0*pi*(sige(nest)**2)/(gain*cf(nest)**2)
c     ebg=1.d0/(pi*(sige(nest)**2)*((gain*hh)**2))
c     ebg=4.d0*pi*(sige(nest)**2)/(gain*cf(nest))**2
c
c
c     bri=1.d0/(gain*cf(nest))
c     bri=1.d0/snr(nest)
c     bri=1.d0/dsqrt(gain*cf(nest))
c     bri=sss/dsqrt(gain*cf(nest))
c     bri=1.d0/snr(nest)
c
c
c     fai=0.d0
c     fai=dsqrt(8.d0*pi*gain*bcg(nest))*(bri**2)
c     fai=bri*dsqrt(2.d0)/(1.d0+dsqrt(dble(npix(nest)/nbcg(nest))))
c     fai=bri*dsqrt(2.d0)/(1.d0+dsqrt(ebg/nbcg(nest)))
c     fai=dsqrt(8.d0*pi*npix(nest)*(bcgs(nest))**2)*sss**2/
c    ?(gain*cf(nest))
c     fai=bri*dsqrt(2.d0)/(1.d0+dsqrt(ebg/nbcg(nest)))
c     fai=0.d0
c     fai=bri*dsqrt(2.d0)/(1.d0+dsqrt(ebg/nbcg(nest)))
c
c     erall=sss*dsqrt(bri**2+fai**2)
c     erall=dsqrt(bri**2+fai**2)
c     erall=sss*dsqrt(bri**2+fai**2)




c
c     Smallest limit error CGA
c
c
c     sss=sige(nest)
c     bri=1.d0/snr(nest)
c     ebg=4.d0*pi*(sige(nest)**2)/(gain*cf(nest))**2
c     fai=bri*dsqrt(2.d0)/(1.d0+dsqrt(ebg/nbcg(nest)))
c     erall=sss*dsqrt(bri**2+fai**2)
c     eg12=1.d0/dsqrt(1.d0-(exce(nest))**2)
c     eg22=dsqrt(1.d0-(exce(nest))**2)
c     ex=dsqrt((dcos(tet)**2)*eg12+(dsin(tet)**2)*eg22)*erall
c     ey=dsqrt((dcos(tet)**2)*eg22+(dsin(tet)**2)*eg12)*erall




c
c     PGM (x,y) error
c


      erall=dsqrt(pi)*cra(nest)/snr(nest)

      eg12=1.d0/dsqrt(1.d0-(exce(nest))**2)
      eg22=dsqrt(1.d0-(exce(nest))**2)
      ex=dsqrt((dcos(tet)**2)*eg12+(dsin(tet)**2)*eg22)*erall
      ey=dsqrt((dcos(tet)**2)*eg22+(dsin(tet)**2)*eg12)*erall



      expix(nest)=ex
      eypix(nest)=ey

      sgcc(nest)=sig
      fgcc(nest)=fc

      angle(nest)=dan(nest)


      sigg=seng(nest)/(2.d0*1.177410023d0)
c     volum(nest)=2.d0*pi*altu(nest)*sigg**2

      volum(nest)=pc

      obtype(nest)='r'


c     write (999,*) k,r/sgcc(nest),r/sgcc(nest)


      ENDIF
c

c     write(801,"('circle(',2(f8.2,','),f8.2,')')") xob(nest),yob(nest),
c    ?cra(nest)


 57   continue

c     stop





c
c     Determines the (x,y) center of rounded-shaped objects input by
c     the user
c


      do 59 k=1,nmcir


      xc=xcir(k)
      yc=ycir(k)
      r=lacir(k)


      rann=2.d0*r


      call prexy (idimx,idimy,idiobs,icofsp,pixmat,imagem,sv,ior,gain,
     ?coefx,xc,yc,r,rann,qua,fc,fs,nfs,pc,a,snra,u20,u02,u11,eg1,eg2,
     ?tet,sig,exc,ix1,ix2,iy1,iy2,nx,ny,ng,modpsf,ierro,gramal)


      if (ierro.eq.1) then
      ierro=0
      go to 59
      endif
c

c     uu20(k)=u20
c     uu02(k)=u02
c     uu11(k)=u11
c
c     seg1(k)=eg1
c     seg2(k)=eg2

c
c     The 2D PSF fit
c


      IF (modpsf.ne.1) THEN


      call psf (idimx,idimy,idiobs,icofsp,pixmat,imagem,fapiro,dlimro,
     ?plimro,icorox,nx,ny,ix1,ix2,iy1,iy2,r,xc,yc,ex,ey,sig,hh,fc,pc,fs,
     ?nfs,u20,u02,u11,eg1,eg2,tet,exc,iwr,modpsf,ierro,resx,sv,ior,xest,
     ?yest,xp,coefx,u,v,w,wsig,floor,qua)


      if (ierro.eq.1) then
      ierro=0
      go to 59
      endif

c     if (sig.gt.r) go to 59

      if (xc.lt.ix1-0.5d0) go to 59
      if (xc.gt.ix2+0.5d0) go to 59
      if (yc.lt.iy1-0.5d0) go to 59
      if (yc.gt.iy2+0.5d0) go to 59


      ELSE

c
c     PRAIA Photogravicenter Method (PGM)
c

      xod=xc
      yod=yc


      DO kk=1,5


      if (ix1.lt.1) ix1=1
      if (iy1.lt.1) iy1=1
 
      if (ix2.gt.nx) ix2=nx
      if (iy2.gt.ny) iy2=ny

      xi=xod
      yi=yod


      call subar (ida,idimx,idimy,sv,ior,ixb,iyb,pixmat,nx,ny,xi,yi,r,
     ?ix1,ix2,iy1,iy2,xod,yod,nnbb)


      ix1=xod-r-1
      ix2=xod+r+1

      iy1=yod-r-1
      iy2=yod+r+1
 

      ENDDO


cc    ex=dsqrt((sig/dsqrt(pc))**2+8.d0*pi*(fc+fs**2)*sig**4/pc)
c
c     ex=sig/snra
c
c     ey=ex
c
c
c     ex=dsqrt((dcos(tet)**2)*eg1**2+(dsin(tet)**2)*eg2**2)/snra
c     ey=dsqrt((dcos(tet)**2)*eg2**2+(dsin(tet)**2)*eg1**2)/snra
c
c     ex=ex*ferpgm
c     ey=ey*ferpgm
c
c
c     eg12=(r**2)/dsqrt(1.d0-exc**2)
c     eg22=dsqrt(1.d0-exc**2)*r**2
c     ex=dsqrt((dcos(tet)**2)*eg12+(dsin(tet)**2)*eg22)
c     ey=dsqrt((dcos(tet)**2)*eg22+(dsin(tet)**2)*eg12)
c     ex=dsqrt(pi)*ex/snra
c     ey=dsqrt(pi)*ey/snra


c
c     PGM (x,y) error
c


      erall=dsqrt(pi)*r/snra

      eg12=1.d0/dsqrt(1.d0-(exce(nest))**2)
      eg22=dsqrt(1.d0-(exce(nest))**2)
      ex=dsqrt((dcos(tet)**2)*eg12+(dsin(tet)**2)*eg22)*erall
      ey=dsqrt((dcos(tet)**2)*eg22+(dsin(tet)**2)*eg12)*erall



      ENDIF


c
c     Stores object data
c

      nest=nest+1



      xid(nest)=xcir(k)
      yid(nest)=ycir(k)

      idx1(nest)=ix1
      idx2(nest)=ix2
      idy1(nest)=iy1
      idy2(nest)=iy2

      idlado(nest)=r
      npix(nest)=a
      bcg(nest)=fc
      bcgs(nest)=fs
      nbcg(nest)=nfs
      dan(nest)=tet
      exce(nest)=exc
      sige(nest)=sig
      seg1(nest)=eg1
      seg2(nest)=eg2
      snr(nest)=snra
      cf(nest)=pc
      cra(nest)=r
      cann(nest)=rann

      uu20(nest)=u20
      uu02(nest)=u02
      uu11(nest)=u11




      IF (modpsf.ne.1) THEN

c
c     2D PSF models
c


      cfwhm=2.d0*sig*1.177410023d0

c     seeing=cfwhm*scala
      seeing=cfwhm


      xob(nest)=xc
      yob(nest)=yc
      ilado(nest)=r
      seng(nest)=seeing

      altu(nest)=hh
      ialtu(nest)=nest

      expix(nest)=ex
      eypix(nest)=ey
      sgcc(nest)=sig
      fgcc(nest)=fc

      angle(nest)=tet


      sigg=seng(nest)/(2.d0*1.177410023d0)
c     volum(nest)=2.d0*pi*altu(nest)*sigg**2

      volum(nest)=cf(nest)

      obtype(nest)='r'


      ELSE


c
c     PRAIA Photogravicenter Method (PGM)
c


      fwhm=2.d0*sig*1.177410023d0

c     fwhm=fwhm*2.d0


c     seeing=fwhm*scala
      seeing=fwhm


      xob(nest)=xod
      yob(nest)=yod
      ilado(nest)=r
      seng(nest)=seeing

      ixc=xob(nest)
      iyc=yob(nest)

      altu(nest)=pixmat(ixc,iyc)-fc
      ialtu(nest)=nest


c     ex=dsqrt(a)/snra
c     ey=ex
c
c
c     ex=sig/snr(nest)
c
c     ex=dsqrt((sig/dsqrt(pc))**2+8.d0*pi*(fc+fs**2)*sig**4/pc)
c
c     ey=ex



      expix(nest)=ex
      eypix(nest)=ey


      sgcc(nest)=sig

      fgcc(nest)=fc

      angle(nest)=tet


      sigg=seng(nest)/(2.d0*1.177410023d0)
c     volum(nest)=2.d0*pi*altu(nest)*sigg**2

      volum(nest)=pc

      obtype(nest)='r'




      ENDIF



c

 59   continue




c
c     Determines the (x,y) center of candidate trace-shaped objects
c     automatically identified by PRAIA by the ERF PSF model
c



      if (ifittr.ne.1)  go to 2107



      write (*,*)
      write (*,*)
      write (*,*) '(x,y) measurements: trace-shaped objects'
      write (*,*)


c
c     Adjustment results for debugging 
c

      open (96,file=ires)

c     write(96,*)'altura,bx,by,sigma*scala,seeing,tetha,dlenght*sc
c    ?ala,fundo,ex*scala,ey*scala,dlenght*altura'
      write(96,*)'altura,bx,by,sigma,seeing,tetha,dlenght*sc
     ?ala,fundo,ex*scala,ey*scala,dlenght*altura'
      write (96,*)

c

      do 217 k=1,ntrac


      xc=xctra(k)
      yc=yctra(k)
      rx=dmalat(k)
      ry=dmilat(k)
      ang=agtra(k)
      fc=bcgt(k)
      sb=bcgst(k)
      



c
c     The Error Function (ERF) PSF model fit of a trace-shaped object
c



      call trace (idiobs,idimx,idimy,icofsp,pixmat,imagem,fapitr,dlimtr,
     ?plimtr,nx,ny,xc,yc,rx,ry,ang,fc,sb,fotceu,lpert,contag,ior,nval,
     ?icotr,altura,bx,by,sigma,tetha,dlenght,fundo,ex,ey,ierro,itx,ity)


      tfwhm=2.d0*sigma*1.177410023d0

c     seeing=tfwhm*scala
      seeing=tfwhm


      nest=nest+1

      xob(nest)=bx
      yob(nest)=by
      ilado(nest)=dlenght
      seng(nest)=seeing

      altu(nest)=altura*dlenght
      ialtu(nest)=nest

      expix(nest)=ex
      eypix(nest)=ey
      sgcc(nest)=sigma
      fgcc(nest)=fundo

      cra(nest)=dlenght


      angle(nest)=tetha

      sigg=seng(nest)/(2.d0*1.177410023d0)
      volum(nest)=2.d0*pi*altu(nest)*sigg**2


      obtype(nest)='t'


 217  continue



c
c     Determines the (x,y) center of trace-shaped objects
c     input by the user, by fitting with the ERF PSF model
c


c
c     Trace fitting is temporarily suspended.
c

      nmtra=0


      do 218 k=1,nmtra


      xc=xtra(k)
      yc=ytra(k)
      rx=xlatra(k)
      ry=ylatra(k)
      ang=angtra(k)
      fc=bcgtr(k)
      sb=bcgstr(k)

    

c
c     The Error Function (ERF) PSF model fit of a trace-shaped object
c


      call trace (idiobs,idimx,idimy,icofsp,pixmat,imagem,fapitr,dlimtr,
     ?plimtr,nx,ny,xc,yc,rx,ry,ang,fc,sb,fotceu,lpert,contag,ior,nval,
     ?icotr,altura,bx,by,sigma,tetha,dlenght,fundo,ex,ey,ierro,itx,ity)



      tfwhm=2.d0*sigma*1.177410023d0

c     seeing=tfwhm*scala
      seeing=tfwhm


      nest=nest+1

      xob(nest)=bx
      yob(nest)=by
      ilado(nest)=dlenght
      seng(nest)=seeing

      altu(nest)=altura*dlenght
      ialtu(nest)=nest

      expix(nest)=ex
      eypix(nest)=ey
      sgcc(nest)=sigma
      fgcc(nest)=fundo

      cra(nest)=dlenght


      angle(nest)=tetha


      sigg=seng(nest)/(2.d0*1.177410023d0)
      volum(nest)=2.d0*pi*altu(nest)*sigg**2

      obtype(nest)='t'



 218  continue

c


 2107 continue


c
c     Eliminates multiple measurements of the same object (if any) when the
c     (x,y) center of the measurement falls inside the aperture of another
c     measurement. The object with the largest aperture is preserved.
c


      do ii=1,nest
      iflag(ii)=0
      enddo


      n=0


      do 65 ii=1,nest

c     sii=cra(ii)/2.d0

      sii=seng(ii)/2.d0


      do 63 jj=1,nest

      if (ii.eq.jj) go to 63

      d=dsqrt((xob(jj)-xob(ii))**2+(yob(jj)-yob(ii))**2)

c     sjj=seng(jj)/2.d0
c     sii=seng(ii)/2.d0


c     sjj=cra(jj)/2.d0


      sjj=seng(jj)/2.d0


      if (d.gt.sjj.and.d.gt.sii) go to 63

c
c     The center of a measurement falls inside the aperture of
c     another measurement. keeps only the one with the largest aperture.
c
c


      n=n+1
      ior1(n)=jj
c     nval1(n)=1000*cra(jj)
      nval1(n)=1000*(expix(jj)**2+eypix(jj)**2)


 63   continue


      if (n.eq.0) go to 65

      n=n+1
      ior1(n)=ii
c     nval1(n)=1000*cra(ii)
      nval1(n)=1000*(expix(ii)**2+eypix(ii)**2)



c
c     Orders common measurements from smaller to larger apertures
c


      call ordem (idiobs,n,ior1,nval1)


c
c     Flags all the smaller apertures. Only the measurement with the
c     largest aperture is preserved.
c


c     do k=1,n-1
c     kk=ior1(k)
c     iflag(kk)=1
c     enddo

      do k=2,n
      kk=ior1(k)
      iflag(kk)=1
      enddo




      n=0

 65   continue


c
c     Stores only non-flagged measurements
c


      nstar=nest

      nest=0

      do 70 k=1,nstar

      if (iflag(k).ne.0) go to 70

      nest=nest+1


      xid(nest)=xid(k)
      yid(nest)=yid(k)
      idx1(nest)=idx1(k)
      idx2(nest)=idx2(k)
      idy1(nest)=idy1(k)
      idy2(nest)=idy2(k)
      idlado(nest)=idlado(k)
      npix(nest)=npix(k)
      bcg(nest)=bcg(k)
      bcgs(nest)=bcgs(k)
      nbcg(nest)=nbcg(k)
      dan(nest)=dan(k)
      exce(nest)=exce(k)
      sige(nest)=sige(k)
      seg1(nest)=seg1(k)
      seg2(nest)=seg2(k)
      snr(nest)=snr(k)
      cf(nest)=cf(k)
      cra(nest)=cra(k)
      cann(nest)=cann(k)
      uu20(nest)=uu20(k)
      uu02(nest)=uu02(k)
      uu11(nest)=uu11(k)


      xob(nest)=xob(k)
      yob(nest)=yob(k)
      ilado(nest)=ilado(k)
      seng(nest)=seng(k)

      altu(nest)=altu(k)
      ialtu(nest)=ialtu(k)

      expix(nest)=expix(k)
      eypix(nest)=eypix(k)
      sgcc(nest)=sgcc(k)
      fgcc(nest)=fgcc(k)

      angle(nest)=angle(k)

      volum(nest)=volum(k)

      obtype(nest)=obtype(k)


      ialtu(nest)=nest
c     nval(nest)=1000.d0*volum(nest)

c

      if (obtype(nest).eq.'t') then

      tetha=angle(nest)*radgra

      altura=altu(nest)
      bx=xob(nest)
      by=yob(nest)
c     sigma=sgcc(nest)*scala
      sigma=sgcc(nest)
      seeing=seng(nest)
c     dlen=ilado(nest)*scala
      dlen=ilado(nest)
      fundo=fgcc(nest)
c     ex=expix(nest)*scala
c     ey=eypix(nest)*scala
      ex=expix(nest)
      ey=eypix(nest)
      dlena=dlen*altura

      write (96,*) altura,bx,by,sigma,seeing,tetha,
     ?dlen,fundo,ex,ey,dlena


      endif



 70   continue


      if (ifittr.eq.1) close (96)


c
c     Eliminates false-positive object identifications
c     that survived the detection filters and the (x,y)
c     centering (PGM or PSF fitting)
c
c
c     The elimination consists in checking objects with
c     too small seeing with respect to the whole
c     distribution.
c
c     The average and standard deviation of the seeing
c     for all objects is determined using quartile
c     statistics to select appropriate seeing points by
c     the use of subroutine "quarte".
c
c     A 5 sigma threshold is used to eliminate outliers
c     with too small seeing. Large seeing objects are
c     not eliminated.
c
c

      fsmall=5.d0

      do k=1,nest
      ior(k)=k
      marca(k)=0
      enddo

      call dordem (idiobs,nest,ior,seng)


      dn=dble(nest)

      nq=idnint(dn*(1.d0-qua))

      call quarte (idiobs,nest,ior,seng,nq,n1,n2)

      n=0

      do i=n1,n2
      n=n+1
      sv(n)=seng(ior(i))
      enddo

      call avevar(sv,n,ave,var)

      var=dsqrt(var)


c     write (*,*)
c     write (*,*) 'ave = ',ave
c     write (*,*) 'sd  = ',var
c     write (*,*)


      do k=1,nest

      dd=(ave-seng(k))/var

      if (dd.gt.fsmall) marca(k)=1

      enddo


      nstar=nest

      nest=0

      do 71 k=1,nstar

      if (marca(k).ne.0) go to 71

      nest=nest+1


      xid(nest)=xid(k)
      yid(nest)=yid(k)
      idx1(nest)=idx1(k)
      idx2(nest)=idx2(k)
      idy1(nest)=idy1(k)
      idy2(nest)=idy2(k)
      idlado(nest)=idlado(k)
      npix(nest)=npix(k)
      bcg(nest)=bcg(k)
      bcgs(nest)=bcgs(k)
      nbcg(nest)=nbcg(k)
      dan(nest)=dan(k)
      exce(nest)=exce(k)
      sige(nest)=sige(k)
      seg1(nest)=seg1(k)
      seg2(nest)=seg2(k)
      snr(nest)=snr(k)
      cf(nest)=cf(k)
      cra(nest)=cra(k)
      cann(nest)=cann(k)
      uu20(nest)=uu20(k)
      uu02(nest)=uu02(k)
      uu11(nest)=uu11(k)


      xob(nest)=xob(k)
      yob(nest)=yob(k)
      ilado(nest)=ilado(k)
      seng(nest)=seng(k)

      altu(nest)=altu(k)
      ialtu(nest)=ialtu(k)

      expix(nest)=expix(k)
      eypix(nest)=eypix(k)
      sgcc(nest)=sgcc(k)
      fgcc(nest)=fgcc(k)

      angle(nest)=angle(k)

      volum(nest)=volum(k)

      obtype(nest)=obtype(k)

      kind(nest)='BOIA1'


      ialtu(nest)=nest
c     nval(nest)=1000.d0*volum(nest)


 71   continue



c
c     Stores original (x,y) measurements without FDP
c


      do i=1,nest

      xoob(i)=xob(i)
      yoob(i)=yob(i)

      enddo




c
c     Ordering from fainter to brightest measured object
c     by flux
c


c     call ordem (idiobs,nest,ialtu,nval)

      call dordem (idiobs,nest,ialtu,cf)



c
c     Minimum number of objects not reached ?
c

c     write (*,*) 'nnn = ',nest

c     stop


c     if (nest.lt.4) go to 60




c
c     Remaking astrometry mode
c



 150  continue

      if (iastro.eq.0) go to 399



c
c     Uploads field data from xy field file (Remaking astrometry modes only)
c


      write (*,*)
      write (*,*)
      write (*,*) 'Remaking astrometry mode. Uploading FOV data ...'
      write (*,*)
      write (*,*)




      daj=0.d0

      fumag=0.d0
      fumag2=0.d0

c

      open (33,file=infits)

c

      do j=1,idiobs


      read (33,470,end=151) xob(j),yob(j),seng(j),altu(j),fgcc(j),gumag,
     ?gumag2,cmgu(j),omag(j),xmgu,xmgj,xmgh,xmgk,oemag,oemag,ermgj,
     ?ermgh,ermgk,pma,pmd,epma,epmd,expix(j),eypix(j),era,ede,alfsig,
     ?delsig,nstart,nfinal,alsi2,desi2,ktira,xra(j),yde(j),iuth,iutm,
     ?sut,iutano,iutmes,iutdia,dj,iexps,ichfil,imfits,mchobj,nx,ny



c
c     Stores original (x,y) measurements given in the xy file
c


      xoob(j)=xob(j)
      yoob(j)=yob(j)

c

      xra(j)=xra(j)*15.d0


c
c     For mixed objects from different frames, like in mosaics of
c     fields, finds the average observation epoch of the mosaic
c
c     If it is a single field, the resulting average equals the
c     original fixed value.
c


      cj=j

      daj=(daj*(cj-1.d0)+(dj-dj2000))/cj


c
c     For mixed objects from different frames, like in mosaics of
c     fields, takes the average of the mean and of the dispersion of
c     the magnitude's sky background
c
c     If it is a single field, the resulting averages equal the
c     original fixed values.
c


      fumag=(fumag*(cj-1.d0)+gumag)/cj
      fumag2=(fumag2*(cj-1.d0)+gumag2)/cj


      enddo


 151  close (33)

      nest=j-1

      dj=daj+dj2000

      epoj=2000d0+(dj-dj2000)/365.25d0



c
c     Updates the UTC gregorian instant for matching the averaged JD epoch      
c

      djm=dj-dj2000

      call iau_jd2cal (dj2000,djm,iutano,iutmes,iutdia,fd,jjj)

      hora=fd*24.d0

      iuth=hora
      iutm=(hora-iuth)*60.d0
      sut=((hora-iuth)*60.d0-iutm)*60.d0


c
c     FOV (RA,Dec) center and limits (Remaking astrometry modes 1 and 2 only)
c
c
c     Extracts or estimates the (RA,Dec) limits and the center of the FOV. 
c
c
c     rac = RA  center in degrees
c     dec = Dec center in degrees
c     
c     ramax = maximum RA in the FOV in degrees
c     ramin = minimum RA in the FOV in degrees
c
c     demax = maximum Dec in the FOV in degrees
c     demin = minimum Dec in the FOV in degrees
c
c
c
c     Here, the FOV's (RA,Dec) limits are not expanded or contracted by a factor
c     given by the user. The FOV's (RA,Dec) true limits are directly determined
c     from the stars' (RA,Dec)s obtained in the previous (RA,Dec) reduction.
c
c
c


      write (*,*)
      write (*,*)
      write (*,*) 'Remaking astrometry mode. Estimating tangent point an
     ?d FOV sizes ...'
      write (*,*)
      write (*,*)


      call fov_12 (idiobs,nest,xra,yde,rac,dec,ramin,ramax,demin,demax,
     ?iah,iam,sa,isig,idg,idm,ds)



c
c
c     Remaking astrometry of xy fields, tangent plane technique option.
c
c
c     Tangent plane technique. Takes (X,Y) standard coordinates from (RA,DEC)
c     as if they were measured (x,y), and proceed with (RA,DEC) reduction with
c     respect to the reference catalogues.
c
c     This reduction procedure is mandatory in the case that the xy files come
c     from a field mosaic reduction with PRAIA, were the stamped (x,y) in the
c     files are meaningless 
c     
c


      if (iastro.eq.2) then


      xxmin=+1.d14
      yymin=+1.d14


      grac=grarad*rac
      gdec=grarad*dec


      do i=1,nest

      bra=xra(i)*grarad
      bde=yde(i)*grarad

      d=dexy(bra,bde,grac,gdec)
      xx=xpad(bra,bde,grac)/d
      yy=ypad(bra,bde,grac,gdec)/d

      xob(i)=xx*radgra*3600.d0
      yob(i)=yy*radgra*3600.d0

   
      if (xob(i).lt.xxmin) xxmin=xob(i)

      if (yob(i).lt.yymin) yymin=yob(i)


      enddo


      do i=1,nest

      xob(i)=xob(i)-xxmin+1.d0
      yob(i)=yob(i)-yymin+1.d0

      enddo


      endif




c
c     All astrometry modes.
c
c
c     Determines typical FWHM, using average and standard deviation of the
c     two middle quartiles (border quartiles cut in 20%) of the FWHM
c     distribution of the objects.
c

 399  continue



      percs=0.2d0

      do i=1,nest
      contag(i)=seng(i)
      enddo

      n=nest


      call quartl (idiobs,ior1,n,percs,contag)

      call avsdev (idiobs,n,contag,fwhm,sigfwh)




c
c     All astrometry modes.
c
c
c     Field Distortion Pattern correction. 
c
c
c     In astrometry option modes 0 and 1, units are in pixels.
c
c     In astrometry option mode 2 (tangent plane technique),
c     units are in arcseconds.
c
c
c 

      write (*,*)
      write (*,*)
      write (*,*) 'Field Distortion Pattern. Applying corrections (if an
     ?y) to the measurements  ...'
      write (*,*)



      if (nfdp.eq.0) go to 125

      if (kfdp.gt.nfdp) kfdp=nfdp


c
c     FDP mode 1, mask in pixel (astrometry modes 0 or 1) or in
c     arcsec units (astrometry mode 2, tangent plane technique)
c


      if (mfdp.eq.1) then



      do i=1,nest


      if (iastro.ne.2) then

      do j=1,nfdp
      ior1(j)=j
      nval1(j)=1000*((xob(i)-coordx(j))**2+(yob(i)-coordy(j))**2)
      enddo

      else

      do j=1,nfdp
      ior1(j)=j
      nval1(j)=1000*(((xra(i)-coordx(j))*dcos(grarad*yde(i)))**2+
     ?(yde(i)-coordy(j))**2)
      enddo


      endif


      
      call ordem (idiobs,nfdp,ior1,nval1)


      ad0=0.d0
      cadx=0.d0
      cady=0.d0

      do k=1,kfdp

      m=ior1(k)

      if (iastro.ne.2) then

      di=dsqrt((xob(i)-coordx(m))**2+(yob(i)-coordy(m))**2)

      else

      di=dsqrt(((xra(i)-coordx(m))*dcos(grarad*yde(i)))**2+
     ?(yde(i)-coordy(m))**2)

      endif

      ad0=ad0+1.d0/di**2
      cadx=cadx+adx(m)/di**2
      cady=cady+ady(m)/di**2

      enddo

      cadx=cadx/ad0
      cady=cady/ad0

      xob(i)=xob(i)-cadx
      yob(i)=yob(i)-cady

      xfdp(i)=cadx
      yfdp(i)=cady

      enddo


      endif




c
c     FDP correction using 3rd degree polynomial distortion model
c
c     FDP mode 2.
c
c     For all astrometry modes
c
c



      if (mfdp.eq.2) then

      ndgr=3

      do i=1,nest

      if (iastro.eq.2) then

      x=xra(i)
      y=yde(i)

      else

      x=xob(i)
      y=yob(i)

      endif



      dx=fpol(icofsp,ndgr,x,y,adx,nterms,i,nest)
      dy=fpol(icofsp,ndgr,x,y,ady,nterms,i,nest)



      xob(i)=xob(i)-dx
      yob(i)=yob(i)-dy

      xfdp(i)=dx
      yfdp(i)=dy


      enddo

      endif



c
c
c     FDP correction using 3rd degree polynomial distortion model
c
c     FDP mode 3 (FITS header keywords general procedure) or
c     mode 4 (DES-DECAM FITS header keywords procedure)
c
c     Only for astrometry mode 0 (needs FITS images)
c    
c



      if (mfdp.eq.3 .or. mfdp.eq.4) then

      if (iastro.ne.0) go to 125 


      ndgr=3


      if (mfdp.eq.3) then
      sccx=adx(2)
      sccy=ady(2)
      endif


      if (mfdp.eq.4) then
      sccx=adx(2)*dsqrt(cdx(1)**2+cdx(2)**2)
      sccy=ady(2)*dsqrt(cdy(1)**2+cdy(2)**2)
      endif

c

      do i=1,nest


      if (mfdp.eq.3) then

      x=xob(i)
      y=yob(i)

      endif



c
c     Linear transformation in DES procedure
c

      if (mfdp.eq.4) then

      xx=xob(i)-crpx
      yy=yob(i)-crpy


      x=cdx(1)*xx+cdx(2)*yy
      y=cdy(1)*xx+cdy(2)*yy

      endif

c

      if (mfdp.eq.3) then


      dx=fpol(icofsp,ndgr,x,y,adx,nterms,i,nest)/sccx
      dy=fpol(icofsp,ndgr,y,x,ady,nterms,i,nest)/sccy



c     xob(i)=xob(i)-dx
c     yob(i)=yob(i)-dy


      xfdp(i)=xob(i)-dx
      yfdp(i)=yob(i)-dy


      xob(i)=dx
      yob(i)=dy



      endif

c

      if (mfdp.eq.4) then


      dx=fpol(icofsp,ndgr,x,y,adx,nterms,i,nest)
      dy=fpol(icofsp,ndgr,y,x,ady,nterms,i,nest)


      xob(i)=crpx-dy/sccy
      yob(i)=crpy+dx/sccx


c     xob(i)=crpx-dy*3600.d0/scala
c     yob(i)=crpy+dx*3600.d0/scala



      xfdp(i)=xob(i)-xoob(i)
      yfdp(i)=yob(i)-yoob(i)


      endif


c


      enddo

      endif



c



 125  continue



c
c     Reports the number of measured objects in the FOV
c


      write (*,*)
      write (*,*)
      write (*,*)
      write (*,*) 'Total number of measured objects in the FOV = ',nest
      write (*,*)
      write (*,*)



c
c
c     FOV (RA,Dec) center and limits (astrometry of images, mode 0 only).
c
c
c
c     Extracts or estimates the (RA,Dec) limits and tangent point (the center
c     so to speak) of the FOV. Also extracts the instant of observation in JD
c     and Julian year, among other FOV auxiliary information.
c
c
c     rac = RA  center in degrees
c     dec = Dec center in degrees
c     
c     ramax = maximum RA in the FOV in degrees
c     ramin = minimum RA in the FOV in degrees
c
c     demax = maximum Dec in the FOV in degrees
c     demin = minimum Dec in the FOV in degrees
c
c
c
c     The FOV's (RA,Dec) limits may be expanded or contracted by a factor
c     given by the user. If the factor is 1 the limits are unchanged.
c
c
c 




      if (iastro.eq.0) then


      write (*,*)
      write (*,*)
      write (*,*) 'Retrieving tangent point and FOV data ...'
      write (*,*)
      write (*,*)


c     call fov_0 (centro,infits,expfov,nx,ny,scala,rac,dec,ramin,ramax,
c    ?demin,demax,iah,iam,sa,isig,idg,idm,ds,iuth,iutm,sut,iutano,
c    ?iutmes,iutdia,dj,epoj,iexps,ichfil,mchobj)


c
c     The preliminary FOV sizes were set by the user
c


      call fov_0 (centro,infits,expfav,nnnnnx,nnnnny,scola,rac,dec,
     ?ramin,ramax,demin,demax,iah,iam,sa,isig,idg,idm,ds,iuth,iutm,sut,
     ?iutano,iutmes,iutdia,dj,epoj,iexps,ichfil,mchobj)



      endif




c

      write (*,*)
      write (*,*)
      write (*,*) 'Data extraction of reference catalogues:'
      write (*,*)


c
c     Stores centers and Vizier data for future use of repeated FOVs
c

      kat=kat+1

      rack(kat)=rac
      deck(kat)=dec

      fcat2m(kat)=''
      fcatga(kat)=''


      dddmin=+1.d14
      mamama=0

      do lalala=1,kat-1

      aux=(rack(kat)-rack(lalala))**2+(deck(kat)-deck(lalala))**2

      if (aux.lt.boxviz) then

      if (aux.lt.dddmin) then
      dddmin=aux
      mamama=lalala
      endif
 
      endif

      enddo


      if (mamama.ne.0) then

      fcat2m(kat)=fcat2m(mamama)
      fcatga(kat)=fcatga(mamama)

      endif


c
c
c     Picks up 2MASS stars from a (RA,Dec) region around the CCD
c     center with twice its estimated sizes     
c
c
c     - ra2ma,de2ma in degrees: (RA,Dec) from 2MASS PSC
c     - extracted magnitudes: J, H and K
c
c
c     Extraction is made by an internet Vizier query.
c

      write (*,*)
      write (*,*) '2MASS. Querying Vizier ...'
      write (*,*)

      call stmass (idiobs,idi2ma,rac,dec,ramin,ramax,demin,demax,ra2ma,
     ?de2ma,dmgj,dmgh,dmgk,emgj,emgh,emgk,ddj2,n2mass,kat,fcat2m)




c
c
c     Picks up stars from the Gaia DR3 catalogue.
c
c
c
c
c     - rag1,deg1: (RA,Dec) ICRS, ephoch J2016.0 (Vizier Distribution)
c     - erag1, edeg1: (RA,Dec) position error in arcsec for JD CCD observation epoch
c     - pmag1: RA proper motion (without the cosDec factor) in arcsec/year
c     - pmdg1: Dec proper motion in arcsec/year
c     - epmag1, epmdg1: proper motion errors in arcsec/year
c     - plg1: parallax in arcsec
c     - eplg1: parallax error in arcsec
c     - rvg1: radial velocity in Km/s
c     - cg1mgg: Gaia magnitude in the G band
c     - g2mag1,g2mag2: min max mag G cutoff
c     - kpm:  key to cut stars without proper motions
c     - kpl:  key to cut stars without parallax
c     - kdup: key to cut stars with dupliticy
c     - mtira: flags stars to be cut by kpm, kpl or kdup keys
c     - ngaia1: number of extracted Gaia stars
c
c
c     Extraction is made by an internet Vizier query.
c


      write (*,*)
      write (*,*)
      write (*,*) 'Gaia DR3. Querying Vizier ...'
      write (*,*)


      call gaia (idiobs,idigai,epoj,rac,dec,ramin,ramax,demin,demax,
     ?g2mag1,g2mag2,kpm,kpl,kdup,rag1,deg1,erag1,edeg1,pmag1,pmdg1,
     ?epmag1,epmdg1,plg1,eplg1,rvg1,cg1mgg,ngaia1,mtira,kat,fcatga)


c

      if (mamama.eq.0) then

      katrm=katrm+1

      rmcat2(katrm)=fcat2m(kat)
      rmcatg(katrm)=fcatga(kat)

      endif



c
c     Picks up Ephemeris catalogue reference positions.
c
c
c     - raeph,deeph: RA,Dec in degrees at epoch epoj of the CCD observation
c     - eraeph, edeeph: (RA,Dec) position error in arcsec for JD observation epoch
c     - pmrap: RA proper motion (multiplied by dcosD factor) in arcsec/year
c     - pmdep: Dec proper motion in arcsec/year
c     - epmrap, epmdep: proper motion errors in arcsec/year
c     - udmgp: magnitude V Johnston
c     - nucacp: number of extracted Ephemeris Catalogue objects
c
c
c
c     call ephem (idiobs,epraia,epoj,rac,dec,ramin,ramax,demin,demax,
c    ?raeph,deeph,eraeph,edeeph,pmrap,pmdep,epmrap,epmdep,udmgp,nucacp)
c
c
c     Ephemeris catalogue does not furnish J, H and K magnitudes and errors
c
c
 
      do i=1,nucaup
      udmgjp(i)=d99
      udmghp(i)=d99
      udmgkp(i)=d99
      edmgjp(i)=d99
      edmghp(i)=d99
      edmgkp(i)=d99
      enddo
 
 


c
c
c     Picks up stars from the User Catalogue (PRAIA format)
c
c
c     - raucs,deucs: (RA,Dec) in degrees at epoch epoj of the CCD observation
c     - eraucs, edeucs: (RA,Dec) position error in arcsec for JD CCD observation epoch
c     - pmras: RA proper motion (multiplied by the cosD factor) in arcsec/year
c     - pmdes: Dec proper motion in arcsec/year
c     - epmras, epmdes: proper motion errors in arcsec/year
c     - udmg_s ...: J, H and K magnitudes from 2MASS in the user catalogue (code 99 if absent)
c     - udmgs:  star magnitude in the user catalogue
c     - nucaus: number of extracted User Catalogue objects
c


      if (iuserc.eq.1) then

      write (*,*)
      write (*,*)

      call cuser (idiobs,cpraia,epoj,rac,dec,ramin,ramax,demin,demax,
     ?raucs,deucs,eraucs,edeucs,pmras,pmdes,epmras,epmdes,udmgjs,udmghs,
     ?udmgks,udmgs,nucaus)

      endif





c
c
c     Picks up target data
c
c
c     tara,tade => (RA,Dec) in degrees
c
c     tadist    => Target distance (km)
c
c     tamag     => Target magnitude
c
c     tafase    => Target solar phase angle (degrees)
c
c     tarad     => Target radius (km)
c
c     tawl      => Target wavelength (um)
c
c     iobalv    => Target label
c
c     nuta      => number of targets in the image
c
c
c

      write (*,*)
      write (*,*)
      write (*,*) 'Data extraction of targets ...'
      write (*,*)


      call target (idiobs,idin50,ialvos,dj,tbox,tara,tade,tadist,tamag,
     ?tafase,tarad,tawl,iobalv,nuta)

      if (nuta.eq.0) ifindt=2



c
c     Recognizes the Gaia DR3 stars among those identified in the FOV
c
c     (Remake astrometry, modes 1 and 2)
c


      if (iastro.eq.0) go to 755

c

      write (*,*)
      write (*,*)
      write (*,*) 'Remaking astrometry mode. Identifying Gaia DR3 stars 
     ?in FOV data ...'
      write (*,*)
      write (*,*)


c     boxerr=(erpix*scala)**2

      boxerr=erpix/3600.d0


      do i=1,idiobs
      idga1(i)=0
      enddo

      nga1=0


      do 750 i=1,ngaia1

      do 749 j=1,nest


      dx=dcos(deg1(i)*grarad)*(xra(j)-rag1(i))
      dy=yde(j)-deg1(i)


      if (dabs(dx).gt.boxerr) go to 749
      if (dabs(dy).gt.boxerr) go to 749


      idga1(j)=i

      nga1=nga1+1


      go to 750


 749  continue

 750  continue


      write (*,*)
      write (*,*)
      write (*,*) 'Gaia DR3 identification concluded.'
      write (*,*)
      write (*,*)



c

      go to 760


c
c    Recognizes the Gaia DR3 stars among those identified in the FOV
c
c    Astrometry of images (mode 0)
c


 755  continue


      write (*,*)
      write (*,*)
      write (*,*) 'Identifying Gaia DR3 stars among the measured objects
     ? ...'
      write (*,*)
      write (*,*)


c
c     Automatic identification of Gaia reference stars and automatic
c     computation of the pixel scale by triangulations.
c


      call idxyad (idiobs,idigai,icofsp,xest,yest,xp,yp,carx,cary,scala,
     ?erpix,rac,dec,nbcat,nbmed,nest,ialtu,xob,yob,ngaia1,idga1,rag1,
     ?deg1,cg1mgg,xold,yold,xrag1,ydeg1,ireflex,ecala,ierro,nga1,wsig,u,
     ?v,w,z,index,nx,ny,xrray,yrray,cvm,xragi,iior,jjor)



      if (ierro.eq.1) then
      write (*,*)
      write (*,*)
      write (*,*) 'Gaia DR3 identification could not be concluded.'
      write (*,*)
      write (*,*)
      ierro=0
      go to 62
      endif

      write (*,*)
      write (*,*)
      write (*,*) 'Gaia DR3 identification concluded.'
      write (*,*)
      write (*,*)


c
c     After the automatic identification of reference Gaia stars and
c     the automatic computation of the pixel scale, retrieves
c     the true sizes of the FOV.
c
c
c
c
c     call fov_0 (centro,infits,expfov,nx,ny,scala,rac,dec,ramin,ramax,
c    ?demin,demax,iah,iam,sa,isig,idg,idm,ds,iuth,iutm,sut,iutano,
c    ?iutmes,iutdia,dj,epoj,iexps,ichfil,mchobj)




c
c     Index 2MASS stars with respect to all measured stars
c     among the measured objects
c
c     Astrometry mode 0 (images)
c


      write (*,*)
      write (*,*)
      write (*,*) 'Indexing 2MASS stars among all measured objects ...'
      write (*,*)
      write (*,*)


c     boxerr=(erpix*scala)**2

      boxerr=erpix/3600.d0


      do 654 i=1,nest

      id2ma(i)=0


      do 653 j=1,n2mass

      dx=dcos(ydeg1(i)*grarad)*(ra2ma(j)-xrag1(i))
      dy=de2ma(j)-ydeg1(i)


      if (dabs(dx).gt.boxerr) go to 653
      if (dabs(dy).gt.boxerr) go to 653


      id2ma(i)=j


      go to 654


 653  continue

 654  continue


      go to 762


c
c     Index 2MASS stars with respect to all measured stars
c     among the measured objects
c
c
c     Astrometry modes 1,2 (remake astrometry)
c


 760  continue


      write (*,*)
      write (*,*)
      write (*,*) 'Indexing 2MASS stars among all measured objects ...'
      write (*,*)
      write (*,*)



c     boxerr=(erpix*scala)**2

      boxerr=erpix/3600.d0


      do 656 i=1,nest

      id2ma(i)=0


      do 655 j=1,n2mass

      dx=dcos(yde(i)*grarad)*(ra2ma(j)-xra(i))
      dy=de2ma(j)-yde(i)


      if (dabs(dx).gt.boxerr) go to 655
      if (dabs(dy).gt.boxerr) go to 655

      id2ma(i)=j

      go to 656

 655  continue

 656  continue




c
c
c     Astrometry of all modes.
c


 762  continue



c
c     Identifies common Gaia DR3 and 2MASS stars for extracting
c     J, H and K 2MASS magnitudes and their errors.
c

 

c     boxerr=(erpix*scala)**2

      boxerr=erpix/3600.d0



      do 650 i=1,ngaia1

      g1mgj(i)=d99
      g1mgh(i)=d99
      g1mgk(i)=d99

      g1emgj(i)=d99
      g1emgh(i)=d99
      g1emgk(i)=d99


      do 649 j=1,n2mass

      dx=dcos(deg1(i)*grarad)*(ra2ma(j)-rag1(i))
      dy=de2ma(j)-deg1(i)


      if (dabs(dx).gt.boxerr) go to 649
      if (dabs(dy).gt.boxerr) go to 649


      g1mgj(i)=dmgj(j)
      g1mgh(i)=dmgh(j)
      g1mgk(i)=dmgk(j)

      g1emgj(i)=emgj(j)
      g1emgh(i)=emgh(j)
      g1emgk(i)=emgk(j)

      go to 650


 649  continue

 650  continue



c
c     Identifies common User catalogue and 2MASS stars
c     for extracting J, H and K 2MASS magnitudes and their
c     errors.
c


      do 652 i=1,nucaus

      udmgjs(i)=d99
      udmghs(i)=d99
      udmgks(i)=d99

      edmgjs(i)=d99
      edmghs(i)=d99
      edmgks(i)=d99



      do 651 j=1,n2mass

      dx=dcos(deucs(i)*grarad)*(ra2ma(j)-raucs(i))
      dy=de2ma(j)-deucs(i)


      if (dabs(dx).gt.boxerr) go to 651
      if (dabs(dy).gt.boxerr) go to 651


      udmgjs(i)=dmgj(j)
      udmghs(i)=dmgh(j)
      udmgks(i)=dmgk(j)

      edmgjs(i)=emgj(j)
      edmghs(i)=emgh(j)
      edmgks(i)=emgk(j)


      go to 652


 651  continue

 652  continue



c
c     Flags common FOV stars with Gaia DR3 flags (mag. range,
c     duplicity, proper motion, parallax) 
c



      do i=1,nest

      itirg1(i)=0

      if (idga1(i).ne.0) then

      if (mtira(idga1(i)).ne.0) itirg1(i)=mtira(idga1(i))

      endif

      enddo


      do i=1,idiobs
      itirgi(i)=itirg1(i)
      enddo


c
c     Primary (RA,Dec) reduction with Gaia DR3
c


      if (iastro.eq.0 .and. ifindg.eq.1) then

      write (*,*)
      write (*,*)
      write (*,*) 'Primary (RA,Dec) reduction with Gaia DR3 ...'
      write (*,*)
      write (*,*)

      else


      write (*,*)
      write (*,*)
      write (*,*) '(RA,Dec) reduction with Gaia DR3 ...'
      write (*,*)
      write (*,*)

      endif


      call posred (idiobs,idigai,icofsp,ireflex,rac,dec,idga1,ngaia1,
     ?rag1,deg1,nest,xob,yob,xest,yest,xp,yp,pcorg1,nstag1,nfing1,xrag1,
     ?ydeg1,erasg1,edesg1,alfsg1,delsg1,alfrg1,delrg1,g1cofx,g1cofy,
     ?g1ecfx,g1ecfy,itirg1,avamg1,dvamg1,ierro,index,iw,wsig,u,v,w,z,
     ?cvm,ior,expix,eypix,exgai,eygai,scala,iastro,xrray,yrray,array,nx,
     ?ny)



c
c     Inverted (RA,Dec) to (x,y) reduction with Gaia DR3
c


      if (iastro.eq.0) then

      call posxy (idiobs,idigai,icofsp,ireflex,rac,dec,idga1,ngaia1,
     ?rag1,deg1,nest,xob,yob,xest,yest,xp,yp,pcorg1,nstagi,nfingi,xragi,
     ?ydegi,erasgi,edesgi,alfsgi,delsgi,alfrgi,delrgi,gicofx,gicofy,
     ?giecfx,giecfy,itirgi,avamgi,dvamgi,ierro,index,iw,wsig,u,v,w,z,
     ?cvm,ior,expix,eypix,scala,iastro,xrray,yrray,array)

      endif


      if (iastro.eq.1) then

      call posxy (idiobs,idigai,icofsp,ireflex,rac,dec,idga1,ngaia1,
     ?rag1,deg1,nest,xob,yob,xest,yest,xp,yp,pcorg1,nstagi,nfingi,xragi,
     ?ydegi,erasgi,edesgi,alfsgi,delsgi,alfrgi,delrgi,gicofx,gicofy,
     ?giecfx,giecfy,itirgi,avamgi,dvamgi,ierro,index,iw,wsig,u,v,w,z,
     ?cvm,ior,exgai,eygai,scala,iastro,xrray,yrray,array)

      endif


      if (iastro.eq.2) then

      call posxy (idiobs,idigai,icofsp,ireflex,rac,dec,idga1,ngaia1,
     ?rag1,deg1,nest,xoob,yoob,xest,yest,xp,yp,pcorg1,nstagi,nfingi,
     ?xragi,ydegi,erasgi,edesgi,alfsgi,delsgi,alfrgi,delrgi,gicofx,
     ?gicofy,giecfx,giecfy,itirgi,avamgi,dvamgi,ierro,index,iw,wsig,u,v,
     ?w,z,cvm,ior,exgai,eygai,scala,iastro,xrray,yrray,array)

      endif



c
c     Verifies the exact number of Gaia DR3 catalogue stars that are
c     present within the exact FOV area 
c


      n=0

      nn=0


      do i=1,ngaia1


      if(xragi(i)-alfsgi/scala.ge.0.5d0.and.xragi(i)+alfsgi/scala.le.nx+
     ?0.5d0.and.ydegi(i)-delsgi/scala.ge.0.5d0.and.ydegi(i)+delsgi/scala
     ?.le.ny+0.5d0) then


      n=n+1


      if (mtira(i).eq.0) nn=nn+1

      endif

      enddo


c

      if (iastro.eq.0 .and. ifindg.eq.1) then

      write (*,*)
      write (*,*) 'Primary Gaia DR3 numbers:'
      write (*,*)

      endif


      write (*,*)
      write (*,*) 'Gaia DR3: No. of all extracted catalogue stars (flagg
     ?ed or not)                       = ',ngaia1
      write (*,*) 'Gaia DR3: Exact No. of extracted catalogue stars with
     ?in the true FOV (flagged or not) = ',n
      write (*,*) 'Gaia DR3: Exact No. of extracted catalogue stars with
     ?in the true FOV (not flagged)    = ',nn
      write (*,*) 'Gaia DR3: No. of identified common measured stars in 
     ?the FOV (flagged or not)         = ',nga1
      write (*,*) 'Gaia DR3: No. of  utilized  common measured stars in 
     ?the FOV                          = ',nfing1
      write (*,*) 'Gaia DR3: No. of eliminated common measured stars in 
     ?the FOV                          = ',nstag1-nfing1
      write (*,*) 'Total number of (x,y) measured objects in the FOV    
     ?                                 = ',nest
      write (*,*)
      write (*,*)




c
c     Astrometry of all modes.
c
c
c     Identifies User catalogue stars in the FOV
c



      IF (iuserc.eq.1) THEN


      write (*,*)
      write (*,*)
      write (*,*) 'Identifying User Catalogue reference objects among th
     ?e measured objects ...'
      write (*,*)
      write (*,*)



      do i=1,idiobs
      iducs(i)=0
      enddo

c

      icoucs=0

c     errou=erpix*scala

      errou=erpix/3600.d0


      do 415 i=1,nucaus
      do 414 j=1,nest

      dx=dcos(deucs(i)*grarad)*(xrag1(j)-raucs(i))
      dy=ydeg1(j)-deucs(i)


      if (dabs(dx).gt.errou) go to 414
      if (dabs(dy).gt.errou) go to 414

      iducs(j)=i

      icoucs=icoucs+1

      go to 415

 414  continue
 415  continue

      write (*,*)
      write (*,*)
      write (*,*) 'Identification of User Catalogue reference objects co
     ?ncluded.'
      write (*,*)
      write (*,*)



      ENDIF





c
c     Astrometry of all modes
c
c     Identifies Ephemeris Catalogue objects in the FOV
c


      IF (iephec.eq.1) THEN


      do i=1,idiobs
      ideph(i)=0
      enddo

c

c     write (*,*)
c     write (*,*)
c     write (*,*) 'Identifying Ephemeris Catalogue reference objects amo
c    ?ng the measured objects ...'
c     write (*,*)
c     write (*,*)

      icoucp=0

c     errou=erpix



c     write (*,*)
c     write (*,*)
c     write (*,*) 'Identification of Ephemeris Catalogue reference objec
c    ?ts concluded.'
c     write (*,*)
c     write (*,*)



      if (iastro.eq.0 .and. ifinde.eq.1) then

      write (*,*)
      write (*,*) 'Primary Ephemeris catalogue numbers:'
      write (*,*)

      endif


      write (*,*)
      write (*,*) 'Ephem: extracted catalogue objects = ',nucacp
      write (*,*) 'Ephem: identified objects in field = ',icoucp
      write (*,*)
      write (*,*)


      ENDIF



c
c     Astrometry of all modes.
c
c
c     (RA,Dec) reduction with User catalogue
c


      IF (iuserc.eq.1) THEN




      if (iastro.eq.0 .and. ifindu.eq.1) then

      write (*,*)
      write (*,*)
      write (*,*) 'Primary (RA,Dec) reduction with User Catalogue ...'
      write (*,*)
      write (*,*)

      else


      write (*,*)
      write (*,*)
      write (*,*) '(RA,Dec) reduction with User Catalogue ...'
      write (*,*)
      write (*,*)

      endif


      do i=1,idiobs
      itiras(i)=0
      enddo



      call posred (idiobs,idiobs,icofsp,ireflex,rac,dec,iducs,nucaus,
     ?raucs,deucs,nest,xob,yob,xest,yest,xp,yp,pcorus,nstars,nfinas,
     ?xraucs,ydeucs,eras,edes,alfsis,delsis,alfres,delres,coefxs,coefys,
     ?ecofxs,ecofys,itiras,avams,dvams,ierro,index,iw,wsig,u,v,w,z,cvm,
     ?ior,expix,eypix,exusr,eyusr,scala,iastro,xrray,yrray,array,nx,ny)



c
c     Inverted (RA,Dec) to (x,y) reduction with the User catalogue
c

      do i=1,idiobs
      itirai(i)=0
      enddo



      if (iastro.eq.0) then

      call posxy (idiobs,idiobs,icofsp,ireflex,rac,dec,iducs,nucaus,
     ?raucs,deucs,nest,xob,yob,xest,yest,xp,yp,pcorus,nstari,nfinai,
     ?xrauci,ydeuci,erai,edei,alfsii,delsii,alfrei,delrei,coefxi,coefyi,
     ?ecofxi,ecofyi,itirai,avami,dvami,ierro,index,iw,wsig,u,v,w,z,cvm,
     ?ior,expix,eypix,scala,iastro,xrray,yrray,array)

      endif


      if (iastro.eq.1) then

      call posxy (idiobs,idiobs,icofsp,ireflex,rac,dec,iducs,nucaus,
     ?raucs,deucs,nest,xob,yob,xest,yest,xp,yp,pcorus,nstari,nfinai,
     ?xrauci,ydeuci,erai,edei,alfsii,delsii,alfrei,delrei,coefxi,coefyi,
     ?ecofxi,ecofyi,itirai,avami,dvami,ierro,index,iw,wsig,u,v,w,z,cvm,
     ?ior,exusr,eyusr,scala,iastro,xrray,yrray,array)

      endif


      if (iastro.eq.2) then

      call posxy (idiobs,idiobs,icofsp,ireflex,rac,dec,iducs,nucaus,
     ?raucs,deucs,nest,xoob,yoob,xest,yest,xp,yp,pcorus,nstari,nfinai,
     ?xrauci,ydeuci,erai,edei,alfsii,delsii,alfrei,delrei,coefxi,coefyi,
     ?ecofxi,ecofyi,itirai,avami,dvami,ierro,index,iw,wsig,u,v,w,z,cvm,
     ?ior,exusr,eyusr,scala,iastro,xrray,yrray,array)

      endif


c
c     Verifies the exact number of User catalogue stars that are
c     present within the exact FOV area 
c



      n=0

      do i=1,nucaus

      if(xrauci(i)-alfsii/scala.ge.0.5d0.and.xrauci(i)+alfsii/scala.le.
     ?nx+0.5d0.and.ydeuci(i)-delsii/scala.ge.0.5d0.and.ydeuci(i)+delsii/
     ?scala.le.ny+0.5d0) n=n+1

      enddo


      if (iastro.eq.0 .and. ifindu.eq.1) then

      write (*,*)
      write (*,*) 'Primary User catalogue numbers:'
      write (*,*)

      endif


      write (*,*)
      write (*,*) 'USER: extracted catalogue stars   = ',nucaus
      write (*,*) 'USER: exact No. of stars in FOV   = ',n
      write (*,*) 'USER: identified stars in FOV     = ',icoucs
      write (*,*) 'USER:  utilized  stars in FOV     = ',nfinas
      write (*,*) 'USER: eliminated stars in FOV     = ',nstars-nfinas
      write (*,*) 'Total measured objects in the FOV = ',nest
      write (*,*)
      write (*,*)


      ENDIF



c
c     Astrometry of all modes.
c
c
c     (RA,Dec) reduction with Ephemeris catalogue
c


      IF (iephec.eq.1) THEN


      if (iastro.eq.0 .and. ifinde.eq.1) then

      write (*,*)
      write (*,*)
      write (*,*) 'Primary (RA,Dec) reduction with Ephemeris Catalogue .
     ?..'
      write (*,*)
      write (*,*)

      else


      write (*,*)
      write (*,*)
      write (*,*) '(RA,Dec) reduction with Ephemeris Catalogue ...'
      write (*,*)
      write (*,*)

      endif


      do i=1,idiobs
      itirap(i)=0
      enddo


      call posred (idiobs,idiobs,icofsp,ireflex,rac,dec,ideph,nucacp,
     ?raucp,deucp,nest,xob,yob,xest,yest,xp,yp,pcorup,nstarp,nfinap,
     ?xraeph,ydeeph,erap,edep,alfsip,delsip,alfrep,delrep,coefxp,coefyp,
     ?ecofxp,ecofyp,itirap,avamp,dvamp,ierro,index,iw,wsig,u,v,w,z,cvm,
     ?ior,expix,eypix,exeph,eyeph,scala,iastro,xrray,yrray,array,nx,ny)


c
c     Inverted (RA,Dec) to (x,y) reduction with the Ephemeris catalogue
c

      do i=1,idiobs
      itirpi(i)=0
      enddo


      if (iastro.eq.0) then

      call posxy (idiobs,idiobs,icofsp,ireflex,rac,dec,ideph,nucacp,
     ?raucp,deucp,nest,xob,yob,xest,yest,xp,yp,pcorup,nstapi,nfinpi,
     ?xraepi,ydeepi,erpi,edpi,alfspi,delspi,alfrpi,delrpi,ciefxp,ciefyp,
     ?ecifxp,ecifyp,itirpi,avampi,dvampi,ierro,index,iw,wsig,u,v,w,z,
     ?cvm,ior,expix,eypix,scala,iastro,xrray,yrray,array)

      endif


      if (iastro.eq.1) then

      call posxy (idiobs,idiobs,icofsp,ireflex,rac,dec,ideph,nucacp,
     ?raucp,deucp,nest,xob,yob,xest,yest,xp,yp,pcorup,nstapi,nfinpi,
     ?xraepi,ydeepi,erpi,edpi,alfspi,delspi,alfrpi,delrpi,ciefxp,ciefyp,
     ?ecifxp,ecifyp,itirpi,avampi,dvampi,ierro,index,iw,wsig,u,v,w,z,
     ?cvm,ior,exeph,eyeph,scala,iastro,xrray,yrray,array)

      endif


      if (iastro.eq.2) then

      call posxy (idiobs,idiobs,icofsp,ireflex,rac,dec,ideph,nucacp,
     ?raucp,deucp,nest,xoob,yoob,xest,yest,xp,yp,pcorup,nstapi,nfinpi,
     ?xraepi,ydeepi,erpi,edpi,alfspi,delspi,alfrpi,delrpi,ciefxp,ciefyp,
     ?ecifxp,ecifyp,itirpi,avampi,dvampi,ierro,index,iw,wsig,u,v,w,z,
     ?cvm,ior,exeph,eyeph,scala,iastro,xrray,yrray,array)

      endif



c
c     Verifies the exact number of Ephemeris catalogue objects that are
c     present within the exact FOV area 
c


      n=0

      do i=1,nucacp

      if(xraepi(i)-alfspi/scala.ge.0.5d0.and.xraepi(i)+alfspi/scala.le.
     ?nx+0.5d0.and.ydeepi(i)-delspi/scala.ge.0.5d0.and.ydeepi(i)+delspi/
     ?scala.le.ny+0.5d0) n=n+1

      enddo


      if (iastro.eq.0 .and. ifinde.eq.1) then

      write (*,*)
      write (*,*) 'Primary Ephemeris catalogue numbers:'
      write (*,*)

      endif


      write (*,*)
      write (*,*) 'Ephem: extracted catalogue objects = ',nucacp
      write (*,*) 'Ephem: exact No. of objects in FOV = ',n
      write (*,*) 'Ephem: identified objects in FOV   = ',icoucp
      write (*,*) 'Ephem:  utilized  objects in FOV   = ',nfinap
      write (*,*) 'Ephem: eliminated objects in FOV   = ',nstarp-nfinap
      write (*,*) 'Total measured objects in the FOV  = ',nest
      write (*,*)
      write (*,*)


      ENDIF



c
c     Storing original identified reference object numbers
c

      kest=nest

      kga1=nga1

      kcoucs=icoucs

      kcoucp=icoucp


c
c     BOIA's post detection and PSF fit of previously undetected
c     Gaia DR3 stars, User catalogue objects, Ephemeris catalogue
c     objects and targets
c
c     Astrometry mode 0 only.
c
c

      if (iastro.ne.0) goto 900


      if (ifindg.ne.1.and.ifindu.ne.1.and.ifinde.ne.1.and.ifindt.ne.1)
     ? go to 900


      if (ifindg.eq.1) then

      write (*,*)
      write (*,*) 'Post-detection and PSF fitting of previously undetect
     ?ed'
      write (*,*) 'Gaia DR3 stars.'
      write (*,*)
      write (*,*)

      endif


      if (ifindu.eq.1) then

      write (*,*)
      write (*,*) 'Post-detection and PSF fitting of previously undetect
     ?ed'
      write (*,*) 'User catalogue stars.'
      write (*,*)
      write (*,*)

      endif


      if (ifinde.eq.1) then

      write (*,*)
      write (*,*) 'Post-detection and PSF fitting of previously undetect
     ?ed'
      write (*,*) 'Ephemeris catalogue objects.'
      write (*,*)
      write (*,*)

      endif


      if (ifindt.eq.1) then

      write (*,*)
      write (*,*) 'Post-detection and PSF fitting of previously undetect
     ?ed'
      write (*,*) 'targets.'
      write (*,*)
      write (*,*)

      endif



c
c     Setting aperture and sky background ring radius (width is fixed
c     and equal to 2 pixels).
c
c     The aperture is defined by a relation with the magnitude of the
c     object:
c
c     1/aperture^2=A+B.mag+C.mag^2
c
c
c     The inner sky background ring radius is also defined by the same
c     relation. 
c
c     The coefficients of the relation are fit by the use of reference
c     objects with previously determined apertures (by the PRAIA's BOIA
c     method) and known magnitudes.
c
c     The reference objects used in the fit, in preference order, are
c     Gaia DR3 stars, User catalogue objects or Ephemeris catalogue
c     objects.
c
c


      gramaz=1.d14

      IF (nga1.gt.3) THEN

      call apemag (idiobs,icofsp,coefam,xest,yest,xp,u,v,w,idga1,cg1mgg,
     ?cra,nest,amag1,amag2,gramal,itirg1)

      call apemag (idiobs,icofsp,coefsm,xest,yest,xp,u,v,w,idga1,cg1mgg,
     ?cann,nest,smag1,smag2,gramaz,itirg1)


      ELSE


      If (icoucs.gt.3) ThEn

      call apemag (idiobs,icofsp,coefam,xest,yest,xp,u,v,w,iducs,udmgs,
     ?cra,nest,amag1,amag2,gramal,itiras)

      call apemag (idiobs,icofsp,coefsm,xest,yest,xp,u,v,w,iducs,udmgs,
     ?cann,nest,smag1,smag2,gramaz,itiras)

      ElSe


      if (icoucp.gt.3) then

      call apemag (idiobs,icofsp,coefam,xest,yest,xp,u,v,w,ideph,udmgp,
     ?cra,nest,amag1,amag2,gramal,itirap)

      call apemag (idiobs,icofsp,coefsm,xest,yest,xp,u,v,w,ideph,udmgp,
     ?cann,nest,smag1,smag2,gramaz,itirap)


      else


      write (*,*)
      write (*,*)
      write (*,*) 'No post detection of reference objects and targets'
      write (*,*) 'could be done. No further PSF fittings will be done.'
      write (*,*) 'The primary (RA,Dec) reduction is final.'
      write (*,*)
      write (*,*)

      go to 900

      endif

      EnDiF

      ENDIF



c
c     Initializing some variables
c


      do i=1,idiobs
      idfg(i)=0
      idfu(i)=0
      idfe(i)=0
      idft(i)=0
      enddo

      n=0

c     efinde=efind**2

c     erpixe=erpix**2

      erpixe=(erpix/scala)**2

      taboxe=(box/scala)**2


c
c     Post-detection of previously undetected Gaia DR3 stars
c


      IF (ifindg.eq.1) THEN


      do 805 i=1,ngaia1

      if (xragi(i)-alfsgi/scala.le.0.5d0) go to 805
 
      if (xragi(i)+alfsgi/scala.ge.nx+0.5d0) go to 805
 
      if (ydegi(i)-delsgi/scala.le.0.5d0) go to 805
 
      if (ydegi(i)+delsgi/scala.ge.ny+0.5d0) go to 805



c     do j=1,nest
c
c     d=(xragi(i)-xob(j))**2+(ydegi(i)-yob(j))**2
c
c     if (d.le.efinde) go to 805
c
c     enddo


      n=n+1

      xf(n)=xragi(i)
      yf(n)=ydegi(i)
      fmag(n)=cg1mgg(i)
      idfg(n)=i

 805  continue

      ENDIF


c
c     Post-detection of previously undetected User catalogue stars
c


      IF (ifindu.eq.1.and.iuserc.eq.1) THEN


      do 810 i=1,nucaus

      if (xrauci(i)-alfsii/scala.le.0.5d0) go to 810

      if (xrauci(i)+alfsii/scala.ge.nx+0.5d0) go to 810

      if (ydeuci(i)-delsii/scala.le.0.5d0) go to 810

      if (ydeuci(i)+delsii/scala.ge.ny+0.5d0) go to 810


c     do j=1,nest
c
c     d=(xrauci(i)-xob(j))**2+(ydeuci(i)-yob(j))**2
c
c     if (d.le.efinde) go to 810
c
c     enddo


c
c     Post-detected User catalogue object is also
c     common to a pos-detected Gaia DR3 star?
c

      do k=1,n

      d=(xrauci(i)-xf(k))**2+(ydeuci(i)-yf(k))**2

c     if (d.le.efinde) then
      if (d.le.erpixe) then
      idfu(k)=i
      go to 810
      endif

      enddo


      n=n+1

      xf(n)=xrauci(i)
      yf(n)=ydeuci(i)
      fmag(n)=udmgs(i)
      idfu(n)=i

 810  continue


      ENDIF



c
c     Post-detection of previously undetected Ephemeris catalogue objects
c


      IF (ifinde.eq.1.and.iephec.eq.1) THEN


      do 815 i=1,nucacp

      if (xraepi(i)-alfspi/scala.le.0.5d0) go to 815

      if (xraepi(i)+alfspi/scala.ge.nx+0.5d0) go to 815

      if (ydeepi(i)-delspi/scala.le.0.5d0) go to 815

      if (ydeepi(i)+delspi/scala.ge.ny+0.5d0) go to 815



c     do j=1,nest
c
c     d=(xraepi(i)-xob(j))**2+(ydeepi(i)-yob(j))**2
c
c     if (d.le.efinde) go to 815
c
c     enddo


c
c     Post-detected Ephemeris catalogue object is also common
c     to a post-detected User catalogue object or Gaia DR3 star?
c


      do k=1,n

      d=(xraepi(i)-xf(k))**2+(ydeepi(i)-yf(k))**2

c     if (d.le.efinde) then
      if (d.le.erpixe) then
      idfe(k)=i
      go to 815
      endif

      enddo


      n=n+1

      xf(n)=xrauci(i)
      yf(n)=ydeuci(i)
      fmag(n)=udmgp(i)
      idfe(n)=i

 815  continue


      ENDIF




c
c     Post-detection of previously undetected targets
c


      IF (ifindt.eq.1) THEN


      do 820 i=1,nuta

      IF (nga1.gt.3) THEN

      call cposxy (icofsp,ireflex,rac,dec,tara(i),tade(i),gicofx,gicofy,
     ?index,xx,yy)

      ELSE

      If (icoucs.gt.3) ThEn

      call cposxy (icofsp,ireflex,rac,dec,tara(i),tade(i),coefxi,coefyi,
     ?index,xx,yy)

      ElSe

      if (icoucp.gt.1) then

      call cposxy (icofsp,ireflex,rac,dec,tara(i),tade(i),coefxe,coefye,
     ?index,xx,yy)

      else

      write (*,*)
      write (*,*)
      write (*,*) 'No post-detection of reference objects or targets'
      write (*,*) 'could be done. No further PSF fittings will be done.'
      write (*,*) 'The primary (RA,Dec) reductions are final.'
      write (*,*)
      write (*,*)

      go to 900

      endif

      EnDiF

      ENDIF


c
c     Displays the (x,y) coordinates of the target in the FOV
c

      write (*,*)
      write (*,*)
      write (*,*)
      write (*,"('Expected FOV coordinates of Target ',i6.6,' :')") i
      write (*,*)
      write (*,"('x (pixels) = ',f7.1)") xx
      write (*,"('y (pixels) = ',f7.1)") yy
      write (*,*)
      write (*,*)
      write (*,*)
      write (*,*)



c
c     Is the target within the FOV ?
c


      if (xx.le.0.5d0) go to 820

      if (xx.ge.nx+0.5d0) go to 820

      if (yy.le.0.5d0) go to 820

      if (yy.ge.ny+0.5d0) go to 820



c
c     Was the target already detected by the PRAIA BOIA method?
c

c     do j=1,nest
c
c     d=(xx-xob(j))**2+(yy-yob(j))**2
c
c     if (d.le.efinde) go to 820
c
c     enddo


c
c     Post-detected target is also common to a post-detected
c     Gaia DR3 star, User catalogue or Ephemeris Catalogue object?
c



      do k=1,n

      d=(xx-xf(k))**2+(yy-yf(k))**2

c     if (d.le.efinde) then
      if (d.le.taboxe) then
      idft(k)=i
      go to 820
      endif

      enddo


      n=n+1

      xf(n)=xx
      yf(n)=yy
      fmag(n)=tamag(i)
      idft(n)=i

 820  continue


      ENDIF


      nff=n


c
c     Determines the (x,y) center of post-detected objects
c     (all post-detections are assumed to be round-shaped objects)
c
c

      indexx=-1

      iterms=3



c
c     Main loop of (xc,yc) measuring of force-detected
c     objects
c


c     do k=70,220
c
c
c     xx=k/10.d0
c
c     if (xx.lt.amag1) xx=amag1
c     if (xx.gt.amag2) xx=amag2
c
c     yy=0.d0
c
c     r=fpol(icofsp,indexx,xx,yy,coefam,iterms,k,nff)
c
c     write (510,*) xx,r
c
c     xx=k/10.d0
c
c     if (xx.lt.smag1) xx=smag1
c     if (xx.gt.smag2) xx=smag2
c
c     yy=0.d0
c
c     rann=fpol(icofsp,indexx,xx,yy,coefsm,iterms,k,nff)
c
c     write (610,*) xx,rann
c
c     enddo


      do 835 k=1,nff


      xc=xf(k)
      yc=yf(k)


c

      if (idft(k).eq.0) then

      errado=erpixe

      else

      errado=taboxe

      endif



c
c     Is the object center immersed in a bad pixel
c     region or ADU count range?
c
c     If so, skip it to the next post-detection candidate.
c


      iix1=xc-1
      iiy1=yc-1
      iix2=xc+1
      iiy2=yc+1

      if (iix1.lt.1) iix1=1
      if (iiy1.lt.1) iiy1=1

      if (iix2.gt.nx) iix2=nx
      if (iiy2.gt.ny) iiy2=ny

      mancha=1

      do ii=iiy1,iiy2
      do jj=iix1,iix2
      if (pixmat(jj,ii).gt.0) mancha=0
      enddo
      enddo


      if (mancha.eq.1) go to 835



c
c     Estimates aperture radius r from the Aperture x Magnitude
c     relation.
c


      xx=fmag(k)

      if (xx.lt.amag1) xx=amag1
      if (xx.gt.amag2) xx=amag2

      yy=0.d0

      r=fpol(icofsp,indexx,xx,yy,coefam,iterms,k,nff)

c     write (512,*) xx,r


c
c     Inner sky background radius is estimated in the same way.
c


c     rann=2.d0*r


      xx=fmag(k)

      if (xx.lt.smag1) xx=smag1
      if (xx.gt.smag2) xx=smag2

      yy=0.d0

      rann=fpol(icofsp,indexx,xx,yy,coefsm,iterms,k,nff)

c     write (612,*) xx,rann


      call prexy (idimx,idimy,idiobs,icofsp,pixmat,imagem,sv,ior,gain,
     ?coefx,xc,yc,r,rann,qua,fc,fs,nfs,pc,a,snra,u20,u02,u11,eg1,eg2,
     ?tet,sig,exc,ix1,ix2,iy1,iy2,nx,ny,ng,modpsf,ierro,gramal)


      if (ierro.eq.1) go to 835



c
c     The 2D PSF fit
c


      IF (modpsf.ne.1) THEN


      call psf (idimx,idimy,idiobs,icofsp,pixmat,imagem,fapiro,dlimro,
     ?plimro,icorox,nx,ny,ix1,ix2,iy1,iy2,r,xc,yc,ex,ey,sig,hh,fc,pc,fs,
     ?nfs,u20,u02,u11,eg1,eg2,tet,exc,iwr,modpsf,ierro,resx,sv,ior,xest,
     ?yest,xp,coefx,u,v,w,wsig,flor,qua)


      if (ierro.eq.1) then
      ierro=0
      go to 835
      endif

c     if (sig.gt.r) go to 835

      if (xc.lt.ix1-0.5d0) go to 835
      if (xc.gt.ix2+0.5d0) go to 835
      if (yc.lt.iy1-0.5d0) go to 835
      if (yc.gt.iy2+0.5d0) go to 835


      ELSE


c
c     PRAIA Photogravicenter Method (PGM)
c

      xod=xc
      yod=yc


      DO kk=1,5


      if (ix1.lt.1) ix1=1
      if (iy1.lt.1) iy1=1
 
      if (ix2.gt.nx) ix2=nx
      if (iy2.gt.ny) iy2=ny

      xi=xod
      yi=yod


      call subar (ida,idimx,idimy,sv,ior,ixb,iyb,pixmat,nx,ny,xi,
     ?yi,r,ix1,ix2,iy1,iy2,xod,yod,nnbb)


      ix1=xod-r-1
      ix2=xod+r+1

      iy1=yod-r-1
      iy2=yod+r+1
 

      ENDDO


      xc=xod
      yc=yod


cc    ex=dsqrt((sig/dsqrt(pc))**2+8.d0*pi*(fc+fs**2)*sig**4/pc)


c     ex=sig/snra
c
c     ey=ex
c
c
c     ex=dsqrt((dcos(tet)**2)*eg1**2+(dsin(tet)**2)*eg2**2)/snra
c     ey=dsqrt((dcos(tet)**2)*eg2**2+(dsin(tet)**2)*eg1**2)/snra
c
c     ex=ex*ferpgm
c     ey=ey*ferpgm
c
c
c
c     ex=dsqrt((dcos(tet)**2)*eg1**2+(dsin(tet)**2)*eg2**2)
c     ey=dsqrt((dcos(tet)**2)*eg2**2+(dsin(tet)**2)*eg1**2)
c
c
c     eeb=1.d0/dsqrt(pc)
c
cc    eef=2.d0*dsqrt(2.d0*pi*fc)/pc
c
cc    eef=2.d0*dsqrt(2.d0*pi*fs)/pc
c
cc    eef=2.d0*dsqrt(2.d0*pi)*fs/pc
c
cc    eef=2.d0*dsqrt(2.d0*pi)/snra
c
cc    eef=2.d0*dsqrt(2.d0*pi*a)*fs/pc
c
c     eef=2.d0*dsqrt(2.d0*pi*nfs)*fs/pc
c
c
c     ex=ex*dsqrt(eeb**2+eef**2)
c     ey=ey*dsqrt(eeb**2+eef**2)
c
c
c
c     ex=ex/dsqrt(eg1*eg2)
c     ey=ey/dsqrt(eg1*eg2)
c
c     ex=ex*dsqrt(a)/snr(nest)
c     ey=ey*dsqrt(a)/snr(nest)
c
c
c     ex=dsqrt((dcos(tet)**2)*eg1**2+(dsin(tet)**2)*eg2**2)
c     ey=dsqrt((dcos(tet)**2)*eg2**2+(dsin(tet)**2)*eg1**2)
c     ex=ex*ferpgm/2.d0
c     ey=ey*ferpgm/2.d0
c     ex=ex/dsqrt(pi)
c     ey=ey/dsqrt(pi)
c     eeb=1.d0/dsqrt(pc)
c     eef=2.d0*dsqrt(2.d0*pi*fc)/pc
c     ex=ex*dsqrt(eeb**2+eef**2)
c     ey=ey*dsqrt(eeb**2+eef**2)
c
c
c
c     ex=dsqrt((dcos(tet)**2)*eg1**2+(dsin(tet)**2)*eg2**2)
c     ey=dsqrt((dcos(tet)**2)*eg2**2+(dsin(tet)**2)*eg1**2)
c     eeb=1.d0/dsqrt(gain*pc)
cc    eef=2.d0*dsqrt(2.d0*pi*gain*fc*4.d0)/(gain*pc)
cc    eef=2.d0*dsqrt(2.d0*pi*a*4.d0)*fs/pc
c     eef=2.d0*dsqrt(2.d0*pi*pi*eg1*eg2*nbip)*fs/pc
c     ex=ex*dsqrt(eeb**2+eef**2)
c     ey=ey*dsqrt(eeb**2+eef**2)
c
c
c     ex=dsqrt((dcos(tet)**2)*eg1**2+(dsin(tet)**2)*eg2**2)
c     ey=dsqrt((dcos(tet)**2)*eg2**2+(dsin(tet)**2)*eg1**2)
c     ex=nbip*ex/snra
c     ey=nbip*ey/snra
c     ex=dsqrt(pi)*ex*ferpgm
c     ey=dsqrt(pi)*ey*ferpgm
c
c
c     ex=dsqrt(pi)*r/snra
c     ey=dsqrt(pi)*r/snra
c
c
c
c     eg12=(r**2)/dsqrt(1.d0-exc**2)
c     eg22=dsqrt(1.d0-exc**2)*r**2
c     ex=dsqrt((dcos(tet)**2)*eg12+(dsin(tet)**2)*eg22)
c     ey=dsqrt((dcos(tet)**2)*eg22+(dsin(tet)**2)*eg12)
c     ex=dsqrt(pi)*ex/snra
c     ey=dsqrt(pi)*ey/snra
c
c
c     area=pi*r**2
c
c
c
c     area=-2.d0*dlog(0.5d0)*pi*eg1*eg2
c
c     eg12=1.d0/dsqrt(1.d0-exc**2)
c     eg22=dsqrt(1.d0-exc**2)
c     ex=dsqrt((dcos(tet)**2)*eg12+(dsin(tet)**2)*eg22)
c     ey=dsqrt((dcos(tet)**2)*eg22+(dsin(tet)**2)*eg12)
c     ex=dsqrt(area)*ex/snra
c     ey=dsqrt(area)*ey/snra
c
c
c
cc    sss=-2.d0*dlog(0.5d0)*sig**2/r
cc
cc    ccc=gain*pc
cc
cc    area=dsqrt((1.d0/ccc+8.d0*pi*fc*(sss**2)/ccc**2)*
cc   ?sss**2)
cc
cc    eg12=1.d0/dsqrt(1.d0-exc**2)
cc    eg22=dsqrt(1.d0-exc**2)
cc    ex=dsqrt((dcos(tet)**2)*eg12+(dsin(tet)**2)*eg22)*area
cc    ey=dsqrt((dcos(tet)**2)*eg22+(dsin(tet)**2)*eg12)*area
c
c
c
c
c
c
c
c     sss=2.d0*dsqrt(-2.d0*dlog(0.5d0))*sig
c
c     sss=r
c
c
c
c
c
c     sss=r
c     bri=1.d0/snra
c     fai=0.d0
c
c
c
c
c     sss=2.d0*dsqrt(-2.d0*dlog(0.5d0))*sig
c     sss=dsqrt(pi)*r
c     sss=2.d0*dsqrt(-2.d0*dlog(0.5d0))*sig/(r/(2.d0*dsqrt(-2.d0*dlog
c    ?(0.5d0))*sig))
c     sss=2.5d0*sig/(r/(2.5d0*sig))
c     sss=2.d0*dsqrt(-2.d0*dlog(0.5d0))*sig
c     sss=sig
c     sss=2.5d0*sig/(r/(2.5d0*sig))
c     sss=r
c     sss=dsqrt(pi)*r
c     sss=sig
c
c
c     hh=pixmat(idnint(xc),idnint(yc))-fc
c
c
c     ebg=4.d0*pi*(sig**2)/pc**2
c     ebg=4.d0*pi*(sig**2)/(gain*pc)**2
c     ebg=1.d0/(((gain*hh)**2)*pi*sig**2)
c     ebg=4.d0*pi*(sig**2)/(gain*pc)**2
c
c
c     bri=1.d0/(gain*pc)
c     bri=1.d0/snra
c     bri=1.d0/dsqrt(gain*pc)
c     bri=sss/dsqrt(gain*pc)
c     bri=1.d0/snra
c
c
c
c     fai=0.d0
c     fai=dsqrt(8.d0*pi*gain*fc)*(bri**2)
c     fai=bri*dsqrt(2.d0)/(1.d0+dsqrt(a/nfs))
c     fai=bri*dsqrt(2.d0)/(1.d0+dsqrt(ebg/nfs))
c     fai=dsqrt(8.d0*pi*a*fs**2)*sss**2/(gain*pc)
c     fai=bri*dsqrt(2.d0)/(1.d0+dsqrt(ebg/nfs))
c     fai=0.d0
c     fai=bri*dsqrt(2.d0)/(1.d0+dsqrt(ebg/nfs))





c
c     Smallest CGA limit error
c
c
c     sss=sig
c     bri=1.d0/snra
c     ebg=4.d0*pi*(sig**2)/(gain*pc)**2
c     fai=bri*dsqrt(2.d0)/(1.d0+dsqrt(ebg/nfs))
c     erall=sss*dsqrt(bri**2+fai**2)
c     eg12=1.d0/dsqrt(1.d0-exc**2)
c     eg22=dsqrt(1.d0-exc**2)
c     ex=dsqrt((dcos(tet)**2)*eg12+(dsin(tet)**2)*eg22)*erall
c     ey=dsqrt((dcos(tet)**2)*eg22+(dsin(tet)**2)*eg12)*erall





c
c     PGM (x,y) error
c


      erall=dsqrt(pi)*r/snra

      eg12=1.d0/dsqrt(1.d0-exc**2)
      eg22=dsqrt(1.d0-exc**2)
      ex=dsqrt((dcos(tet)**2)*eg12+(dsin(tet)**2)*eg22)*erall
      ey=dsqrt((dcos(tet)**2)*eg22+(dsin(tet)**2)*eg12)*erall




      ENDIF



c
c     Is the fitted (xc,yc) center obtained coincident
c     with the (x,y) center of any previous primary or post
c     detection?
c
c     If the answer is NO, a new measurement is obtained
c     and added with the other (x,y) measurements.
c
c
c
c     But if the answer is YES, then keeps the (x,y) measurement
c     and metadata of the measurement with the best (x,y) center
c     precision.
c        
c



      DO j=1,nest


      d=(xc-xob(j))**2+(yc-yob(j))**2


c
c     The object turned out to be an already measured Gaia DR3 star,
c     User catalogue object, Ephemeris catalogue object or target.
c


      IF (d.le.errado) THEN


      dddnew=ex**2+ey**2
 
      dddobj=expix(j)**2+eypix(j)**2

 
      if (dddobj.le.dddnew) go to 835
 
 
      nn=j

      kind(nn)='BOIA2'

      go to 832


      ENDIF


      ENDDO




c
c
c     A brand new (x,y) measurement was found for a Gaia DR3 star,
c     User catalogue object, Ephemeris catalogue object or target.
c
c     Stores the force-detected object data
c


      nest=nest+1


      nn=nest

      kind(nn)='BOIA3'


c
c     Reference catalogue indexes
c

      idga1(nn)=idfg(k)
      iducs(nn)=idfu(k)
      ideph(nn)=idfe(k)


      if (idga1(nn).ne.0) nga1=nga1+1

      if (iducs(nn).ne.0) icoucs=icoucs+1

      if (ideph(nn).ne.0) icoucp=icoucp+1


c
c     Indexing 2MASS to force-detected objects
c

      ipass=0

      if (iducs(nn).ne.0) then
      ra=raucs(iducs(nn))
      de=deucs(iducs(nn))
      ipass=1
      endif

      if (idga1(nn).ne.0) then
      ra=rag1(idga1(nn))
      de=deg1(idga1(nn))
      ipass=1
      endif


      if (ipass.ne.1) go to 832


c     boxerr=(erpix*scala)**2


      boxerr=erpix/3600.d0


      do 831 j=1,n2mass

      dx=dcos(de*grarad)*(ra2ma(j)-ra)
      dy=de2ma(j)-de

      if (dabs(dx).lt.boxerr) go to 831
      if (dabs(dy).lt.boxerr) go to 831

      id2ma(nn)=j

      go to 832

 831  continue

c

 832  continue



c
c     Flushes post-detection data
c


      xid(nn)=xf(k)
      yid(nn)=yf(k)

      idx1(nn)=ix1
      idx2(nn)=ix2
      idy1(nn)=iy1
      idy2(nn)=iy2

      idlado(nn)=r
      npix(nn)=a
      bcg(nn)=fc
      bcgs(nn)=fs
      nbcg(nn)=nfs
      dan(nn)=tet
      exce(nn)=exc
      sige(nn)=sig
      seg1(nn)=eg1
      seg2(nn)=eg2
      snr(nn)=snra
      cf(nn)=pc
      cra(nn)=r
      cann(nn)=rann

      uu20(nn)=u20
      uu02(nn)=u02
      uu11(nn)=u11




      IF (modpsf.ne.1) THEN

c
c     2D PSF models
c


      cfwhm=2.d0*sig*1.177410023d0

c     seeing=cfwhm*scala
      seeing=cfwhm


      xob(nn)=xc
      yob(nn)=yc
      ilado(nn)=r
      seng(nn)=seeing

      altu(nn)=hh
      ialtu(nn)=nn

      expix(nn)=ex
      eypix(nn)=ey
      sgcc(nn)=sig
      fgcc(nn)=fc

      angle(nn)=tet


      sigg=seng(nn)/(2.d0*1.177410023d0)
c     volum(nn)=2.d0*pi*altu(nn)*sigg**2

      volum(nn)=cf(nn)

      obtype(nn)='r'


      ELSE


c
c     PRAIA Photogravicenter Method (PGM)
c


      fwhm=2.d0*sig*1.177410023d0

c     fwhm=fwhm*2.d0

c     seeing=fwhm*scala
      seeing=fwhm


      xob(nn)=xc
      yob(nn)=yc
      ilado(nn)=r
      seng(nn)=seeing

      ixc=xob(nn)
      iyc=yob(nn)

      altu(nn)=pixmat(ixc,iyc)-fc
      ialtu(nn)=nn


c     ex=dsqrt(a)/snra
c     ey=ex

      expix(nn)=ex
      eypix(nn)=ey


      sgcc(nn)=sig
      fgcc(nn)=fc

      angle(nn)=tet


      sigg=seng(nn)/(2.d0*1.177410023d0)
c     volum(nn)=2.d0*pi*altu(nn)*sigg**2

      volum(nn)=pc

      obtype(nn)='r'


      ENDIF

      xoob(nn)=xob(nn)
      yoob(nn)=yob(nn)



c

 835  continue


c     do i=1,nest
c     ior(i)=i
c     enddo
c
c     call dordem (idiobs,nest,ior,cra)
c
c     do i=nest,1,-1
c     k=ior(i)
c     if (idga1(k).ne.0) then
c     dma=cg1mgg(idga1(k))
c     aux=seng(k)
c     aux=aux/scala
c     aux=aux/2.d0
c     aux=aux/1.177410023d0
c     aux1=aux*scala
c     aux=cra(k)/aux
c     write (900,*) i,aux
c     write (900,*) i,xob(k),yob(k),cra(k)/aux,aux1,cra(k),dma
c     endif
c     enddo



c
c     Final (RA,Dec) direct and inverse reductions with
c     post-detections added, Gaia DR3
c


      IF (ifindg.eq.1) THEN



      do i=1,nest

      itirg1(i)=0

      if (idga1(i).ne.0) then

      if (mtira(idga1(i)).ne.0) itirg1(i)=mtira(idga1(i))

      endif

      enddo



      do i=1,idiobs
      itirgi(i)=itirg1(i)
      enddo



      write (*,*)
      write (*,*)
      write (*,*) 'Final (RA,Dec) reduction with Gaia DR3 ...'
      write (*,*)
      write (*,*)



      call posred (idiobs,idigai,icofsp,ireflex,rac,dec,idga1,ngaia1,
     ?rag1,deg1,nest,xob,yob,xest,yest,xp,yp,pcorg1,nstag1,nfing1,xrag1,
     ?ydeg1,erasg1,edesg1,alfsg1,delsg1,alfrg1,delrg1,g1cofx,g1cofy,
     ?g1ecfx,g1ecfy,itirg1,avamg1,dvamg1,ierro,index,iw,wsig,u,v,w,z,
     ?cvm,ior,expix,eypix,exgai,eygai,scala,iastro,xrray,yrray,array,nx,
     ?ny)



c     do i=1,nest
c     ior(i)=i
c     enddo
c
c     call dordem (idiobs,nest,ior,snr)
c
c     j=0
c     do i=1,nest
c     k=ior(i)
c     if (idga1(k).ne.0) then
c     if (itirg1(k).eq.0) then
c     j=j+1
c     write(900,*) j,snr(k),cg1mgg(idga1(k)),exgai(k)*1000,eygai(k)*1000
c    ?,dsqrt(cf(k))
c     endif
c     endif
c     enddo
 
 
c     do i=1,nest
c     ior(i)=i
c     enddo
c
c     call dordem (idiobs,nest,ior,cra)
c
c     do i=nest,1,-1
c     k=ior(i)
c     if (idga1(k).ne.0) then
c     if (itirg1(k).eq.0) then
c     dma=cg1mgg(idga1(k))
c     aux=cra(k)/(seng(k)/scala)
c     write (900,*) dma,seng(k),aux,xob(k),yob(k)
c     endif
c     endif
c     enddo



c
c     Final inverted (RA,Dec) to (x,y) reduction with Gaia DR3
c


      call posxy (idiobs,idigai,icofsp,ireflex,rac,dec,idga1,ngaia1,
     ?rag1,deg1,nest,xob,yob,xest,yest,xp,yp,pcorg1,nstagi,nfingi,xragi,
     ?ydegi,erasgi,edesgi,alfsgi,delsgi,alfrgi,delrgi,gicofx,gicofy,
     ?giecfx,giecfy,itirgi,avamgi,dvamgi,ierro,index,iw,wsig,u,v,w,z,
     ?cvm,ior,expix,eypix,scala,iastro,xrray,yrray,array)


c
c     Verifies the exact final number of Gaia DR3 catalogue stars that
c     are present within the exact FOV area 
c


      n=0

      nn=0

      do i=1,ngaia1


      if(xragi(i)-alfsgi/scala.ge.0.5d0.and.xragi(i)+alfsgi/scala.le.nx+
     ?0.5d0.and.ydegi(i)-delsgi/scala.ge.0.5d0.and.ydegi(i)+delsgi/scala
     ?.le.ny+0.5d0) then


      n=n+1

      if (mtira(i).eq.0) nn=nn+1

      endif

      enddo


      write (*,*)
      write (*,*) 'Final Gaia DR3 numbers:'
      write (*,*)

      write (*,*)
      write (*,*) 'Gaia DR3: No. of all extracted catalogue stars (flagg
     ?ed or not)                       = ',ngaia1
      write (*,*) 'Gaia DR3: Exact No. of extracted catalogue stars with
     ?in the true FOV (flagged or not) = ',n
      write (*,*) 'Gaia DR3: Exact No. of extracted catalogue stars with
     ?in the true FOV (not flagged)    = ',nn
      write (*,*) 'Gaia DR3: No. of identified common measured stars in 
     ?the FOV (flagged or not)         = ',nga1
      write (*,*) 'Gaia DR3: No. of  utilized  common measured stars in 
     ?the FOV                          = ',nfing1
      write (*,*) 'Gaia DR3: No. of eliminated common measured stars in 
     ?the FOV                          = ',nstag1-nfing1
      write (*,*) 'Total number of (x,y) measured objects in the FOV    
     ?                                 = ',nest
      write (*,*)
      write (*,*)




c
c     Final determination of the pixel scale in
c     arcseconds per pixel from Gaia reductions
c

      facx=dble(nx)
      facy=dble(ny)

      x1=g1cofx(1)*radgra*3600.d0
      x2=g1cofx(2)*radgra*3600.d0/facx
      x3=g1cofx(3)*radgra*3600.d0/facy

      y1=g1cofy(1)*radgra*3600.d0
      y2=g1cofy(2)*radgra*3600.d0/facx
      y3=g1cofy(3)*radgra*3600.d0/facy


      scala=dsqrt((x2**2+x3**2+y2**2+y3**2)/2.d0)


      scagai=scala


c
c     Final determination of the pixel scale error in
c     arcseconds per pixel
c

      ex2=g1ecfx(2)*radgra*3600.d0/facx
      ex3=g1ecfx(3)*radgra*3600.d0/facy

      ey2=g1ecfy(2)*radgra*3600.d0/facx
      ey3=g1ecfy(3)*radgra*3600.d0/facy


      ecala=0.5d0*dsqrt(((x2*ex2)**2+(x3*ex3)**2+(y2*ey2)**2+
     ?(y3*ey3)**2))/scala


c
c     FOV orientation angles
c

      cs=(x2+y3)/2.d0
      sn=(x3-y2)/2.d0

      tet=radgra*theta(cs,sn)
      if (ireflex.lt.0) tet=tet+180.d0
      if (tet.gt.360.d0) tet=tet-360.d0


c
c     FOV center
c

      xx=nx/2.d0
      yy=ny/2.d0


      call xyrade (icofsp,nx,ny,xx,yy,rac,dec,index,ireflex,g1cofx,
     ?g1cofy,iah,iam,sa,isig,idg,idm,ds)


      write (ird,37) iah,iam,sa,isig,idg,idm,ds
 37   format(2(i2.2,1x),f7.4,1x,a1,2(i2.2,1x),f6.3)

      if (ird(07:07).eq.' ') ird(07:07)='0'
      if (ird(08:08).eq.' ') ird(08:08)='0'
      if (ird(22:22).eq.' ') ird(22:22)='0'
      if (ird(23:23).eq.' ') ird(23:23)='0'

c

      write (*,*) 
      write (*,*)
      write (*,*)
      write (*,*) 'Final determination of the pixel scale and FOV'
      write (*,*)
      write (*,*)
      write (*,38) scala
 38   format(1x,'Pixel scale value (arcsec/pixel)     = ',f11.8)
      write (*,*)
      write (*,39) ecala
 39   format(1x,'Pixel scale error (arcsec/pixel)     = ',f11.8)
      write (*,*)
      write (*,40) nx*scala/60.d0,ny*scala/60.d0
 40   format(1x,'FOV (arcminutes) = ',f5.1,' x ',f5.1)
      write (*,*)
      write (*,41) tet
 41   format(1x,'East anti-clockwise from X axis (dg) = ',f8.4)
      write (*,*)
      if (ireflex.gt.0) then
      write (*,*) 'North anti-clockwise from East'
      else
      write (*,*) 'North clockwise from East'
      endif
      write (*,*)
      write (*,42) ird
 42   format(1x,'FOV center (RA,Dec) hh mm ss  dg m s = ',a27)
      write (*,*)
      write (*,43) idnint(nx/2.d0),idnint(ny/2.d0)
 43   format(1x,'FOV center (x,y) in pixels           = ',2(i5,1x))
      write (*,*)
      write (*,*)
      write (*,*)


      ENDIF



c
c     Final (RA,Dec) direct and inverse reductions with
c     post-detections added, User catalogue
c


      IF (iuserc.eq.1.and.ifindu.eq.1) THEN



      write (*,*)
      write (*,*)
      write (*,*) 'Final (RA,Dec) reduction with User Catalogue ...'
      write (*,*)
      write (*,*)


      do i=1,idiobs
      itiras(i)=0
      enddo



      call posred (idiobs,idiobs,icofsp,ireflex,rac,dec,iducs,nucaus,
     ?raucs,deucs,nest,xob,yob,xest,yest,xp,yp,pcorus,nstars,nfinas,
     ?xraucs,ydeucs,eras,edes,alfsis,delsis,alfres,delres,coefxs,coefys,
     ?ecofxs,ecofys,itiras,avams,dvams,ierro,index,iw,wsig,u,v,w,z,cvm,
     ?ior,expix,eypix,exusr,eyusr,scala,iastro,xrray,yrray,array,nx,ny)



c
c     Final inverted (RA,Dec) to (x,y) reduction with the User catalogue
c

      do i=1,idiobs
      itirai(i)=0
      enddo


      call posxy (idiobs,idiobs,icofsp,ireflex,rac,dec,iducs,nucaus,
     ?raucs,deucs,nest,xob,yob,xest,yest,xp,yp,pcorus,nstari,nfinai,
     ?xrauci,ydeuci,erai,edei,alfsii,delsii,alfrei,delrei,coefxi,coefyi,
     ?ecofxi,ecofyi,itirai,avami,dvami,ierro,index,iw,wsig,u,v,w,z,cvm,
     ?ior,expix,eypix,scala,iastro,xrray,yrray,array)



c
c     Verifies the exact final number of User catalogue stars that are
c     present within the exact FOV area 
c


      n=0

      do i=1,nucaus

      if(xrauci(i)-alfsii/scala.ge.0.5d0.and.xrauci(i)+alfsii/scala.le.
     ?nx+0.5d0.and.ydeuci(i)-delsii/scala.ge.0.5d0.and.ydeuci(i)+delsii/
     ?scala.le.ny+0.5d0) n=n+1

      enddo


      write (*,*)
      write (*,*) 'Final User catalogue numbers:'
      write (*,*)


      write (*,*)
      write (*,*) 'USER: extracted catalogue stars   = ',nucaus
      write (*,*) 'USER: exact No. of stars in FOV   = ',n
      write (*,*) 'USER: identified stars in field   = ',icoucs
      write (*,*) 'USER:  utilized  stars in FOV     = ',nfinas
      write (*,*) 'USER: eliminated stars in FOV     = ',nstars-nfinas
      write (*,*) 'Total measured objects in the FOV = ',nest
      write (*,*)
      write (*,*)



coefxs,coefys


c
c     Final determination of the pixel scale in
c     arcseconds per pixel from User Catalogue reductions
c

      facx=dble(nx)
      facy=dble(ny)

      x1=coefxs(1)*radgra*3600.d0
      x2=coefxs(2)*radgra*3600.d0/facx
      x3=coefxs(3)*radgra*3600.d0/facy

      y1=coefys(1)*radgra*3600.d0
      y2=coefys(2)*radgra*3600.d0/facx
      y3=coefys(3)*radgra*3600.d0/facy


      scala=dsqrt((x2**2+x3**2+y2**2+y3**2)/2.d0)


      scausr=scala


c
c     Final determination of the pixel scale error in
c     arcseconds per pixel
c

      ex2=coefxs(2)*radgra*3600.d0/facx
      ex3=coefxs(3)*radgra*3600.d0/facy

      ey2=coefys(2)*radgra*3600.d0/facx
      ey3=coefys(3)*radgra*3600.d0/facy


      ecala=0.5d0*dsqrt(((x2*ex2)**2+(x3*ex3)**2+(y2*ey2)**2+
     ?(y3*ey3)**2))/scala


c
c     FOV orientation angles
c

      cs=(x2+y3)/2.d0
      sn=(x3-y2)/2.d0

      tet=radgra*theta(cs,sn)
      if (ireflex.lt.0) tet=tet+180.d0
      if (tet.gt.360.d0) tet=tet-360.d0


c
c     FOV center
c

      xx=nx/2.d0
      yy=ny/2.d0


      call xyrade (icofsp,nx,ny,xx,yy,rac,dec,index,ireflex,coefxs,
     ?coefys,iah,iam,sa,isig,idg,idm,ds)


      write (ird,37) iah,iam,sa,isig,idg,idm,ds
c37   format(2(i2.2,1x),f7.4,1x,a1,2(i2.2,1x),f6.3)

      if (ird(07:07).eq.' ') ird(07:07)='0'
      if (ird(08:08).eq.' ') ird(08:08)='0'
      if (ird(22:22).eq.' ') ird(22:22)='0'
      if (ird(23:23).eq.' ') ird(23:23)='0'

c

      write (*,*) 
      write (*,*)
      write (*,*)
      write (*,*) 'Final determination of the pixel scale and FOV - User
     ? Catalogue'
      write (*,*)
      write (*,*)
      write (*,38) scala
c38   format(1x,'Pixel scale value (arcsec/pixel)     = ',f11.8)
      write (*,*)
      write (*,39) ecala
c39   format(1x,'Pixel scale error (arcsec/pixel)     = ',f11.8)
      write (*,*)
      write (*,40) nx*scala/60.d0,ny*scala/60.d0
c40   format(1x,'FOV (arcminutes) = ',f5.1,' x ',f5.1)
      write (*,*)
      write (*,41) tet
c41   format(1x,'East anti-clockwise from X axis (dg) = ',f8.4)
      write (*,*)
      if (ireflex.gt.0) then
      write (*,*) 'North anti-clockwise from East'
      else
      write (*,*) 'North clockwise from East'
      endif
      write (*,*)
      write (*,42) ird
c42   format(1x,'FOV center (RA,Dec) hh mm ss  dg m s = ',a27)
      write (*,*)
      write (*,43) idnint(nx/2.d0),idnint(ny/2.d0)
c43   format(1x,'FOV center (x,y) in pixels           = ',2(i5,1x))
      write (*,*)
      write (*,*)
      write (*,*)



      ENDIF



c
c     Final (RA,Dec) direct and inverse reductions with
c     post-detections added, Ephemeris catalogue
c


      IF (iephec.eq.1.and.ifinde.eq.1) THEN



      write (*,*)
      write (*,*)
      write(*,*) 'Final (RA,Dec) reduction with Ephemeris catalogue ...'
      write (*,*)
      write (*,*)


      do i=1,idiobs
      itirap(i)=0
      enddo


      call posred (idiobs,idiobs,icofsp,ireflex,rac,dec,ideph,nucacp,
     ?raucp,deucp,nest,xob,yob,xest,yest,xp,yp,pcorup,nstarp,nfinap,
     ?xraeph,ydeeph,erap,edep,alfsip,delsip,alfrep,delrep,coefxp,coefyp,
     ?ecofxp,ecofyp,itirap,avamp,dvamp,ierro,index,iw,wsig,u,v,w,z,cvm,
     ?ior,expix,eypix,exeph,eyeph,scala,iastro,xrray,yrray,array,nx,ny)





c
c     Final inverted (RA,Dec) to (x,y) reduction with the Ephemeris
c     catalogue
c

      do i=1,idiobs
      itirpi(i)=0
      enddo


      call posxy (idiobs,idiobs,icofsp,ireflex,rac,dec,ideph,nucacp,
     ?raucp,deucp,nest,xob,yob,xest,yest,xp,yp,pcorup,nstapi,nfinpi,
     ?xraepi,ydeepi,erpi,edpi,alfspi,delspi,alfrpi,delrpi,ciefxp,ciefyp,
     ?ecifxp,ecifyp,itirpi,avampi,dvampi,ierro,index,iw,wsig,u,v,w,z,
     ?cvm,ior,expix,eypix,scala,iastro,xrray,yrray,array)





c
c     Verifies the exact final number of Ephemeris catalogue objects
c     that are present within the exact FOV area 
c


      n=0

      do i=1,nucacp

      if(xraepi(i)-alfspi/scala.ge.0.5d0.and.xraepi(i)+alfspi/scala.le.
     ?nx+0.5d0.and.ydeepi(i)-delspi/scala.ge.0.5d0.and.ydeepi(i)+delspi/
     ?scala.le.ny+0.5d0) n=n+1

      enddo


      write (*,*)
      write (*,*) 'Final Ephemeris catalogue numbers:'
      write (*,*)


      write (*,*)
      write (*,*) 'Ephem: extracted catalogue objects = ',nucacp
      write (*,*) 'Ephem: exact No. of objects in FOV = ',n
      write (*,*) 'Ephem: identified objects in field = ',icoucp
      write (*,*) 'Ephem:  utilized  objects in FOV   = ',nfinap
      write (*,*) 'Ephem: eliminated objects in FOV   = ',nstarp-nfinap
      write (*,*) 'Total measured objects in the FOV  = ',nest
      write (*,*)
      write (*,*)




c
c     Final determination of the pixel scale in
c     arcseconds per pixel from Ephemeris Catalogue reductions
c

      facx=dble(nx)
      facy=dble(ny)

      x1=coefxp(1)*radgra*3600.d0
      x2=coefxp(2)*radgra*3600.d0/facx
      x3=coefxp(3)*radgra*3600.d0/facy

      y1=coefyp(1)*radgra*3600.d0
      y2=coefyp(2)*radgra*3600.d0/facx
      y3=coefyp(3)*radgra*3600.d0/facy


      scala=dsqrt((x2**2+x3**2+y2**2+y3**2)/2.d0)


      scaeph=scala


c
c     Final determination of the pixel scale error in
c     arcseconds per pixel
c

      ex2=coefxp(2)*radgra*3600.d0/facx
      ex3=coefxp(3)*radgra*3600.d0/facy

      ey2=coefyp(2)*radgra*3600.d0/facx
      ey3=coefyp(3)*radgra*3600.d0/facy


      ecala=0.5d0*dsqrt(((x2*ex2)**2+(x3*ex3)**2+(y2*ey2)**2+
     ?(y3*ey3)**2))/scala


c
c     FOV orientation angles
c

      cs=(x2+y3)/2.d0
      sn=(x3-y2)/2.d0

      tet=radgra*theta(cs,sn)
      if (ireflex.lt.0) tet=tet+180.d0
      if (tet.gt.360.d0) tet=tet-360.d0


c
c     FOV center
c

      xx=nx/2.d0
      yy=ny/2.d0


      call xyrade (icofsp,nx,ny,xx,yy,rac,dec,index,ireflex,coefxp,
     ?coefyp,iah,iam,sa,isig,idg,idm,ds)


      write (ird,37) iah,iam,sa,isig,idg,idm,ds
c37   format(2(i2.2,1x),f7.4,1x,a1,2(i2.2,1x),f6.3)

      if (ird(07:07).eq.' ') ird(07:07)='0'
      if (ird(08:08).eq.' ') ird(08:08)='0'
      if (ird(22:22).eq.' ') ird(22:22)='0'
      if (ird(23:23).eq.' ') ird(23:23)='0'

c

      write (*,*) 
      write (*,*)
      write (*,*)
      write (*,*) 'Final determination of the pixel scale and FOV - Ephe
     ?meris Catalogue'
      write (*,*)
      write (*,*)
      write (*,38) scala
c38   format(1x,'Pixel scale value (arcsec/pixel)     = ',f11.8)
      write (*,*)
      write (*,39) ecala
c39   format(1x,'Pixel scale error (arcsec/pixel)     = ',f11.8)
      write (*,*)
      write (*,40) nx*scala/60.d0,ny*scala/60.d0
c40   format(1x,'FOV (arcminutes) = ',f5.1,' x ',f5.1)
      write (*,*)
      write (*,41) tet
c41   format(1x,'East anti-clockwise from X axis (dg) = ',f8.4)
      write (*,*)
      if (ireflex.gt.0) then
      write (*,*) 'North anti-clockwise from East'
      else
      write (*,*) 'North clockwise from East'
      endif
      write (*,*)
      write (*,42) ird
c42   format(1x,'FOV center (RA,Dec) hh mm ss  dg m s = ',a27)
      write (*,*)
      write (*,43) idnint(nx/2.d0),idnint(ny/2.d0)
c43   format(1x,'FOV center (x,y) in pixels           = ',2(i5,1x))
      write (*,*)
      write (*,*)
      write (*,*)



      ENDIF





 900  continue

c
c     Astrometry of all modes.
c
c
c     Writes statistcs of (RA,Dec) reduction with Gaia DR3 catalogue
c


      write (*,*)
      write (*,*)
      write (*,*) 'Files (Ra,Dec) reduction data and statistics for all 
     ?catalogues ...'
      write (*,*)
      write (*,*)




      if (nstag1.eq.0) nstag1=1

      if (nstag1.lt.nfing1) nfing1=nstag1

      percg1=100.d0*(nstag1-nfing1)/nstag1

c
      open (97,file=redrg1)

 450  read (97,*,end=451)
      go to 450

 451  call backsp (2,nbac,97)

c

      write (97,452) alfsg1,delsg1,nstag1,nfing1,percg1,avamg1,dvamg1,
     ?dj,iah,iam,sa,isig,idg,idm,ds,iexps,ichfil,infits,mchobj,nx,ny,
     ?(g1cofx(i),i=1,icofsp),(g1cofy(i),i=1,icofsp),
     ?(g1ecfx(i),i=1,icofsp),(g1ecfy(i),i=1,icofsp)



 452  format(2(1x,f6.3),2(1x,i4),1x,f6.2,2(1x,f6.3),1x,f16.8,
     ?1x,i2.2,1x,i2.2,1x,f7.4,1x,a1,i2.2,1x,i2.2,1x,f6.3,2x,i4,2x,a20,
     ?2x,a50,1x,a20,2(1x,i5),1x,4(21(1x,es24.16e3)))


c452  format(2(1x,f6.3),2(1x,i4),1x,f6.2,2(1x,f6.3),1x,f16.8,
c    ?1x,i2,1x,i2,1x,f7.4,1x,a1,i2,1x,i2,1x,f6.3,2x,i4,2x,a20,2x,a50,
c    ?1x,a20,2(1x,i5),1x,4(21(1x,f16.13)))


      close (97)




c
c     Astrometry of all modes.
c
c
c     Writes statistcs of (RA,Dec) reduction with User catalogue
c


      if (iuserc.eq.1) then


      if (nstars.eq.0) nstars=1

      if (nstars.lt.nfinas) nfinas=nstars

      percus=100.d0*(nstars-nfinas)/nstars

c
      open (97,file=redrus)

 465  read (97,*,end=466)
      go to 465

 466  call backsp (2,nbac,97)

c

      write (97,452) alfsis,delsis,nstars,nfinas,percus,avams,dvams,dj,
     ?iah,iam,sa,isig,idg,idm,ds,iexps,ichfil,infits,mchobj,nx,ny,
     ?(coefxs(i),i=1,icofsp),(coefys(i),i=1,icofsp),
     ?(ecofxs(i),i=1,icofsp),(ecofys(i),i=1,icofsp)


      close (97)


      endif





c
c     Astrometry of all modes.
c
c
c     Writes statistcs of (RA,Dec) reduction with Ephemeris catalogue
c

      if (iephec.eq.1) then

      if (nstarp.eq.0) nstarp=1
 
      if (nstarp.lt.nfinap) nfinap=nstarp
 
      percp=100.d0*(nstarp-nfinap)/nstarp
 
c

      open (97,file=redrep)
 
 469  read (97,*,end=471)
      go to 469
 
 471  call backsp (2,nbac,97)
 
 
 
      write (97,452) alfsip,delsip,nstarp,nfinap,percp,avamp,dvamp,dj,
     ?iah,iam,sa,isig,idg,idm,ds,iexps,ichfil,infits,mchobj,nx,ny,
     ?(coefxp(i),i=1,icofsp),(coefyp(i),i=1,icofsp),
     ?(ecofxp(i),i=1,icofsp),(ecofyp(i),i=1,icofsp)
 
 
      close (97)
 

      endif


c
c     Astrometry of all modes.
c
c
c     Average sky background counts
c



      write (*,*)
      write (*,*)
      write (*,*) 'Computes and files magnitude data for all catalogues 
     ?...'
      write (*,*)
      write (*,*)



      percs=0.2d0
      ncut=5

      volcor=0.d0

      do i=1,nest
      contag(i)=fgcc(i)
      enddo

      n=nest

      if (n.ge.ncut) call quartl (idiobs,ior1,n,percs,contag)

      call avsdev (idiobs,n,contag,fundo,fundo2)


      ceu2=fundo2*pi*(fwhm/(2.d0*1.177410023d0))**2


c
c     Negative average sky background in ADUs.
c
c     In this case, add a volume correction to all objects. The
c     volume correction corresponds to the amount necessary
c     to get a positive volume for the sky background, if the average
c     sky background was 1 ADU.  
c


      if (fundo.le.0.d0) then

      volcor=(1.d0-fundo)*pi*(fwhm/(2.d0*1.177410023d0))**2

      ceu=pi*(fwhm/(2.d0*1.177410023d0))**2
      
      else

      ceu=fundo*pi*(fwhm/(2.d0*1.177410023d0))**2

      endif



c
c     PSF volumes for astrometry mode 0 - sky background volume correction 
c


      if (iastro.eq.0) then

      do i=1,nest

      volum(i)=volum(i)+volcor

      enddo

      endif


c
c     Retrieves PSF volumes for astrometry modes 1 and 2 
c


      if (iastro.ne.0) then


      do i=1,nest

      sigg=seng(i)/(2.d0*1.177410023d0)

      volum(i)=2.d0*pi*altu(i)*sigg**2+volcor

      enddo

      endif






c
c     Astrometry of all modes.
c
c
c     PSF magnitudes and error, and sky background magnitude
c     in the Ephemeris catalogue system
c

      if (iephec.eq.1) then


      fpmag=d99
      fpmags=d99
      rps2mg=d99

      do i=1,nest
      cudmgp(i)=d99
      enddo


      call magnitudes (d99,idiobs,idiobs,nest,percs,ncut,ior1,nval1,
     ?contag,ceu,ceu2,volum,ideph,udmgp,cudmgp,rps2mg,fpmag,fpmags,
     ?g2mag1,g2mag2,itirap)

      endif


c
c     Astrometry of all modes.
c
c
c     PSF magnitudes and error, and sky background magnitude
c     in the User catalogue system
c



      if (iuserc.eq.1) then


      fsmag=d99
      fsmags=d99
      rss2mg=d99

      do i=1,nest
      cudmgs(i)=d99
      enddo


      call magnitudes (d99,idiobs,idiobs,nest,percs,ncut,ior1,nval1,
     ?contag,ceu,ceu2,volum,iducs,udmgs,cudmgs,rss2mg,fsmag,fsmags,
     ?g2mag1,g2mag2,itiras)

      endif





c
c     Astrometry of all modes.
c
c
c     PSF magnitudes and error, and sky background magnitude
c     in the Gaia DR3 G catalogue system
c



      call magnitudes (d99,idiobs,idigai,nest,percs,ncut,ior1,nval1,
     ?contag,ceu,ceu2,volum,idga1,cg1mgg,ug1mgg,g1smg,g1fmg,g1fmgs,
     ?g2mag1,g2mag2,itirg1)



c
c     Astrometry of all modes.
c
c
c     Updates photometry report
c


      open (99,file=fotrel)

 500  read (99,*,end=501)
      go to 500

 501  call backsp (2,nbac,99)
    


c
c
c
c     - fundo,fundo2: average and sigma of sky background in ADUs
c
c
c     - fpmag,fpmags,rps2mg: average and sigma magnitude of sky background and sigma of 
c                            catalogue magnitude fitting based on the Ephemeris catalogue
c                            system
c
c     - g1fmg,g1fmgs,g1smg : average and sigma magnitude of sky background and sigma of star
c                            catalogue magnitude fitting based on the Gaia DR3 G system
c
c
c     - fsmag,fsmags,rss2mg: average and sigma magnitude of sky background and sigma of star
c                            catalogue magnitude fitting based on the User catalogue system
c
c     - fwhm,sigfwh : average seeing and sigma about this average
c


      write (99,502) fwhm,sigfwh,fundo,fundo2,fpmag,fpmags,rps2mg,g1fmg,
     ?g1fmgs,g1smg,fsmag,fsmags,rss2mg,nest,iexps,ichfil,infits,ichobj,
     ?nx,ny

 502  format(2(1x,f5.3),2(1x,f7.1),9(1x,f6.3),1x,i5,2x,i4,2x,a20,2x,
     ?a50,2x,a20,2(1x,i5))

      close (99)




c
c     Stores astrometric results of individual objects for each field
c     for each catalogue reduction (stores data to the so called xy PRAIA files)
c 


      if (lfdp.eq.1) then

      do i=1,nest

      xob(i)=xoob(i)
      yob(i)=yoob(i)

      enddo

      endif



c
c     Astrometry of all modes.
c
c
c     Outputs PRAIA xy results for Gaia DR3 (RA,Dec) reductions
c


      call results (d99,idiobs,idigai,idi2ma,linxy,nest,kest,ixyg,idga1,
     ?idga1,id2ma,xob,yob,seng,altu,fgcc,g1fmg,g1fmgs,cg1mgg,ug1mgg,
     ?dmgj,dmgh,dmgk,g1smg,emgj,emgh,emgk,pmag1,pmdg1,epmag1,epmdg1,
     ?exgai,eygai,erasg1,edesg1,alfsg1,delsg1,nstag1,nfing1,alfrg1,
     ?delrg1,itirg1,xrag1,ydeg1,iuth,iutm,sut,iutano,iutmes,iutdia,dj,
     ?iexps,ichfil,infits,mchobj,nx,ny,xfdp,yfdp,obtype,kind,scagai)



c     do k=1,nest
c
c     if (idga1(k).ne.0) then
c
c
c     if ((itirg1(k).eq.0.and.nest.gt.1000).or.nest.lt.1001) then
c
c     dma=cg1mgg(idga1(k))
c
c     aux=seng(k)/scala
c
c     ratio=cra(k)/aux
c
c     write (900,*) dma,ratio,seng(k),cra(k)
c
c     endif
c
c
c     endif
c
c     enddo


c
c     Astrometry of all modes.
c
c
c     Outputs PRAIA xy results for User catalogue (RA,Dec) reductions
c

      if (iuserc.eq.1) then

      call results (d99,idiobs,idiobs,idi2ma,linxy,nest,kest,ixyu,iducs,
     ?iducs,id2ma,xob,yob,seng,altu,fgcc,fsmag,fsmags,udmgs,cudmgs,dmgj,
     ?dmgh,dmgk,rss2mg,emgj,emgh,emgk,pmras,pmdes,epmras,epmdes,exusr,
     ?eyusr,eras,edes,alfsis,delsis,nstars,nfinas,alfres,delres,itiras,
     ?xraucs,ydeucs,iuth,iutm,sut,iutano,iutmes,iutdia,dj,iexps,ichfil,
     ?infits,mchobj,nx,ny,xfdp,yfdp,obtype,kind,scausr)

      endif




c
c     Astrometry of all modes.
c
c
c     Outputs PRAIA xy results for Ephemeris catalogue (RA,Dec) reductions
c


      if (iephec.eq.1) then


      call results (d99,idiobs,idiobs,idi2ma,linxy,nest,kest,ixyp,ideph,
     ?ideph,id2ma,xob,yob,seng,altu,fgcc,fpmag,fpmags,udmgp,cudmgp,dmgj,
     ?dmgh,dmgk,rps2mg,emgj,emgh,emgk,pmrap,pmdep,epmrap,epmdep,exeph,
     ?eyeph,erap,edep,alfsip,delsip,nstarp,nfinap,alfrep,delrep,itirap,
     ?raucp,deucp,iuth,iutm,sut,iutano,iutmes,iutdia,dj,iexps,ichfil,
     ?infits,mchobj,nx,ny,xfdp,yfdp,obtype,kind,scaeph)

      endif




c
c     Astrometry of all modes.
c
c
c     Provides observed minus reference position statistics for targets
c     for each of the (RA,Dec) catalogue reductions 
c
c




c
c     Astrometry of all modes.
c
c
c     Ephemeris catalogue
c


      if (iephec.eq.1) then
 
      write (*,*) 
      write (*,*) 
      write (*,*) 
      write (*,*) 
      write (*,*) '  **** Ephemeris catalogue: flushing results ... ***
     ?* '
      write (*,*) 
 
      icato='W'
 
      kob=3
  

      call estat (idiobs,linxy,nest,ixyp,box,nuta,tara,tade,tadist,
     ?tamag,tafase,tarad,tawl,iobalv,iredep,ialvep,malvep,nalvep,obtipo,
     ?band,iau,icato,kob)


      endif



c
c     Astrometry of all modes.
c
c
c     User catalogue
c


      if (iuserc.eq.1) then

      kob=2

      write (*,*) 
      write (*,*) 
      write (*,*) 
      write (*,*) '  **** User Catalogue: flushing results ... **** '
      write (*,*) 

      call estat (idiobs,linxy,nest,ixyu,box,nuta,tara,tade,tadist,
     ?tamag,tafase,tarad,tawl,iobalv,iredus,ialvus,malvus,nalvus,obtipo,
     ?band,iau,icat,kob)

      endif



c
c     Astrometry of all modes.
c
c
c     Gaia DR3 catalogue
c


      write (*,*) 
      write (*,*) 
      write (*,*) 
      write (*,*) 
      write (*,*) '  **** Gaia DR3 : flushing results ... **** '
      write (*,*) 

      icato='W'

      kob=1

      call estat (idiobs,linxy,nest,ixyg,box,nuta,tara,tade,tadist,
     ?tamag,tafase,tarad,tawl,iobalv,iredg1,ialvg1,malvg1,nalvg1,obtipo,
     ?band,iau,icato,kob)



c
c     Flushing the ds9 region files for each
c     reference catalogue
c


      write (*,*) 
      write (*,*) 
      write (*,*)
      write (*,*)
      write (*,*) 'Flushing ds9 region data ...'
      write (*,*)
      write (*,*)



      if (iastro.ne.0) then

      do i=1,nest

c     obtype(i)='r'
      cra(i)=10.d0
      ilado(i)=10

      enddo

      endif


      IF (iastro.ne.0) THEN

c
c     Gaia DR3
c

      n=0

      do 510 i=1,ngaia1

      do j=1,nest
      if(idga1(j).eq.i) go to 510
      enddo


      if(xragi(i)+alfsgi/scagai.ge.0.5d0.and.xragi(i)-alfsgi/scagai.le.
     ?nx+0.5d0.and.ydegi(i)+delsgi/scagai.ge.0.5d0.and.ydegi(i)-delsgi/
     ?scagai.le.ny+0.5d0) then

      n=n+1

      xoob(n+nest)=xragi(i)
      yoob(n+nest)=ydegi(i)

      cra(n+nest)=10.d0

      mxy=''

      write(mxy(233:236),'(i4.4)') mtira(i)

      ixyg(n+nest)=mxy

      endif


 510  continue


      call ids9 (idiobs,linxy,ids9g1,ixyg,obtype,xoob,yoob,cra,ilado,
     ?angle,nest,n)


c
c     User catalogue
c

      if (iuserc.eq.1) then

      n=0

      do 520 i=1,nucaus

      do j=1,nest
      if(iducs(j).eq.i) go to 520
      enddo


      if(xrauci(i)+alfsii/scausr.ge.0.5d0.and.xrauci(i)-alfsii/scausr.
     ?le.nx+0.5d0.and.ydeuci(i)+delsii/scausr.ge.0.5d0.and.ydeuci(i)-
     ?delsii/scausr.le.ny+0.5d0) then


      n=n+1

      xoob(n+nest)=xrauci(i)
      yoob(n+nest)=ydeuci(i)

      cra(n+nest)=10.d0

      mxy=''

      write(mxy(233:236),'(i4.4)') izero

      ixyu(n+nest)=mxy

      endif


 520  continue


      call ids9 (idiobs,linxy,ids9us,ixyu,obtype,xoob,yoob,cra,ilado,
     ?angle,nest,n)

      endif


c
c     Ephemeris catalogue
c
c

      if (iephec.eq.1) then

      n=0
 

      do 530 i=1,nucacp

      do j=1,nest
      if(ideph(j).eq.i) go to 530
      enddo


      if(xraepi(i)+alfspi/scaeph.ge.0.5d0.and.xraepi(i)-alfspi/scaeph.
     ?le.nx+0.5d0.and.ydeepi(i)+delspi/scaeph.ge.0.5d0.and.ydeepi(i)-
     ?delspi/scaeph.le.ny+0.5d0) then


      n=n+1

      xoob(n+nest)=xraepi(i)
      yoob(n+nest)=ydeepi(i)

      cra(n+nest)=10.d0

      mxy=''

      write(mxy(233:236),'(i4.4)') izero

      ixyu(n+nest)=mxy

      endif


 530  continue


      call ids9 (idiobs,linxy,ids9ep,ixyp,obtype,xoob,yoob,cra,ilado,
     ?angle,nest,n)

      endif


      ELSE


c
c     Gaia DR3
c

      n=0

      do 540 i=1,ngaia1

      do j=1,nest
      if(idga1(j).eq.i) go to 540
      enddo


      if(xragi(i)+alfsgi/scagai.ge.0.5d0.and.xragi(i)-alfsgi/scagai.le.
     ?nx+0.5d0.and.ydegi(i)+delsgi/scagai.ge.0.5d0.and.ydegi(i)-delsgi/
     ?scagai.le.ny+0.5d0) then


      n=n+1

      xob(n+nest)=xragi(i)
      yob(n+nest)=ydegi(i)

      cra(n+nest)=10.d0

      mxy=''

      write(mxy(233:236),'(i4.4)') mtira(i)

      ixyg(n+nest)=mxy

      endif


 540  continue

      call ids9 (idiobs,linxy,ids9g1,ixyg,obtype,xob,yob,cra,ilado,
     ?angle,nest,n)


c
c     User catalogue
c

      if (iuserc.eq.1) then

      n=0

      do 550 i=1,nucaus

      do j=1,nest
      if(iducs(j).eq.i) go to 550
      enddo


      if(xrauci(i)+alfsii/scausr.ge.0.5d0.and.xrauci(i)-alfsii/scausr.
     ?le.nx+0.5d0.and.ydeuci(i)+delsii/scausr.ge.0.5d0.and.ydeuci(i)-
     ?delsii/scausr.le.ny+0.5d0) then



      n=n+1

      xob(n+nest)=xrauci(i)
      yob(n+nest)=ydeuci(i)

      cra(n+nest)=10.d0

      mxy=''

      write(mxy(233:236),'(i4.4)') izero

      ixyu(n+nest)=mxy

      endif


 550  continue


      call ids9 (idiobs,linxy,ids9us,ixyu,obtype,xob,yob,cra,ilado,
     ?angle,nest,n)

      endif



c
c     Ephemeris catalogue
c
c

      if (iephec.eq.1) then

      n=0
 

      do 560 i=1,nucacp

      do j=1,nest
      if(ideph(j).eq.i) go to 560
      enddo


      if(xraepi(i)+alfspi/scaeph.ge.0.5d0.and.xraepi(i)-alfspi/scaeph.
     ?le.nx+0.5d0.and.ydeepi(i)+delspi/scaeph.ge.0.5d0.and.ydeepi(i)-
     ?delspi/scaeph.le.ny+0.5d0) then


      n=n+1

      xob(n+nest)=xraepi(i)
      yob(n+nest)=ydeepi(i)

      cra(n+nest)=10.d0

      mxy=''

      write(mxy(233:236),'(i4.4)') izero

      ixyu(n+nest)=mxy

      endif


 560  continue


      call ids9 (idiobs,linxy,ids9ep,ixyp,obtype,xob,yob,cra,ilado,
     ?angle,nest,n)

      endif



      ENDIF




c
c     Astrometry of all modes.
c
c
c     Finalizing the astrometry of the field
c



 62   continue



c
c     Stores flattened image in fits format for debug purposes
c
c
c     do i=1,ny
c     do j=1,nx
c     pixel(j,i)=pixmat(j,i)
c     enddo
c     enddo
c      
c
c     subf='teste.fits'
c     if=83
c     betpix=-32
c     mswap=2
c     scale=1.d0
c     zero=0.d0
c
c
c     call wfits (idimx,idimy,if,subf,betpix,mswap,nx,ny,pixel,scale,
c    ?zero)
c
c



 60   continue


c
c     Erasing downloaded Vizier catalogues from the local
c     directory
c

      erase=''

      erase='rm '


      do m=1,katrm

      erase(4:24)=rmcat2(m)

      call system (erase)

      erase(4:24)=rmcatg(m)
      
      call system (erase)

      enddo

c
      
      return
      
      end





c
c
c     Subroutine pimask
c
c
c     Masks of bad pixels input by the user to be excluded of any
c     image processing. Excluded pixels are marked by setting
c     negative ADU counts for them in the pixel matrix.
c
c
c     Independent of valid negative pixel counts, a fixed (positive
c     or negative) value is added to all the valid pixels in the image,
c     so that the minimum valid pixel count is set to 100 ADUs
c     in the image.
c
c     Masked bad pixels are set to -100 ADU counts in the matrix.
c
c
c     The pixels to be excluded are setup from pixel masks
c     furnished by the user. There are 3 types of pixel masks:
c
c        1 - rectangular areas
c
c        2 - circular areas
c
c        3 - Range of ADU pixel counts
c
c
c
c     There are masks valid for all images, and masks for
c     individual images.
c
c
c     Masks for all images:
c
c     bada - bad pixel mask file with bad range of ADU counts
c
c     badb - bad pixel mask file with bad rectangular regions
c
c     badc - bad pixel mask file with bad regions
c
c
c
c     Masks for individual images:
c
c     ibada - bad pixel mask file with bad range of ADU counts
c
c     ibadb - bad pixel mask file with bad rectangular regions
c
c     ibadc - bad pixel mask file with bad regions
c
c
c
c
c     Last update:   M. Assafin - 19/May/2020
c
c


      subroutine pimask (idin,idimx,idimy,pixmat,imagem,nx,ny,bada,
     ?badb,badc,ibada,ibadb,ibadc)

      implicit real*8 (A-H,O-Z)

      real*4 pixmat(idimx,idimy)
      integer*2 imagem(idimx,idimy)

      character*(idin) bada,badb,badc
      character*(idin+10) ibada,ibadb,ibadc

      character*200 linha

 

c
c     Initializing auxiliary pixel matrix
c

      do k=1,ny
      do j=1,nx
      imagem(j,k)=0
      enddo
      enddo



c
c     Masks bad pixels from original corrupted matrix data
c     (NaN pixels)



c     do    i=1,ny
c     do 99 j=1,nx
c
c     linha=''
c     write (linha,*) pixmat(j,i)
c
c     do k=1,198
c     if (linha(k:k+2).eq.'NaN'.or.linha(k:k+2).eq.'nAn'.or.
c    ?linha(k:k+2).eq.'nan'.or.linha(k:k+2).eq.'NAN') then
c     imagem(j,i)=-20
c     go to 99 
c     endif
c     enddo
c
c  99 continue
c     enddo


c
c
c     Bad pixels, circular regions, all images
c
c     Free format: xc, yc, radius, key
c
c     Bad key:
c               - positive: mask pixels within region 
c               - negative: mask pixels outside region
c
c      
c     Unlimited number of entries
c
c
      

      open (23,file=badc,status='old',err=111)

 110  continue

      read (23,*,end=111) xmask,ymask,rmask,ibad 

      
      if (ibad.eq.0) then
      write (*,*)
      write (*,*) 'Incorrect setup of bad pixels, circular regions.'
      write (*,*) 'Option inside/outside circle region cannot be zero.'
      write (*,*) 'Aborting.'
      write (*,*)
      write (*,*)
      stop
      endif


      
      if (ibad.gt.0) then

      
      ix1=xmask-rmask
      ix2=xmask+rmask
      iy1=ymask-rmask
      iy2=ymask+rmask

      if (ix1.lt.1) ix1=1
      if (iy1.lt.1) iy1=1
      if (ix2.gt.nx) ix2=nx
      if (iy2.gt.ny) iy2=ny


      do ii=iy1,iy2
      do jj=ix1,ix2
      call circul (rmask,xmask,ymask,jj,ii,ichave)
      if (ichave.ge.0) imagem(jj,ii)=-20
      enddo
      enddo

      endif

      
      if (ibad.lt.0) then

      do ii=1,ny
      do jj=1,nx
      call circul (rmask,xmask,ymask,jj,ii,ichave)
      if (ichave.lt.0) imagem(jj,ii)=-20
      enddo
      enddo

      endif

      

      go to 110


 111  close (23)




c
c
c     Bad pixels, circular regions, individual images
c
c     Free format: xc, yc, radius, key
c
c     Bad key:
c               - positive: mask pixels within region 
c               - negative: mask pixels outside region
c
c      
c     Unlimited number of entries
c
c
      

      open (23,file=ibadc,status='old',err=113)

 112  continue

      read (23,*,end=113) xmask,ymask,rmask,ibad 

      
      if (ibad.eq.0) then
      write (*,*)
      write (*,*) 'Incorrect setup of bad pixels, circular regions.'
      write (*,*) 'Option inside/outside circle region cannot be zero.'
      write (*,*) 'Aborting.'
      write (*,*)
      write (*,*)
      stop
      endif


      
      if (ibad.gt.0) then

      
      ix1=xmask-rmask
      ix2=xmask+rmask
      iy1=ymask-rmask
      iy2=ymask+rmask

      if (ix1.lt.1) ix1=1
      if (iy1.lt.1) iy1=1
      if (ix2.gt.nx) ix2=nx
      if (iy2.gt.ny) iy2=ny


      do ii=iy1,iy2
      do jj=ix1,ix2
      call circul (rmask,xmask,ymask,jj,ii,ichave)
      if (ichave.ge.0) imagem(jj,ii)=-1
      enddo
      enddo

      endif

      
      if (ibad.lt.0) then

      do ii=1,ny
      do jj=1,nx
      call circul (rmask,xmask,ymask,jj,ii,ichave)
      if (ichave.lt.0) imagem(jj,ii)=-20
      enddo
      enddo

      endif

      

      go to 112


 113  close (23)





      
c
c     Bad pixels, rectangular regions, all images
c
c     Free format: xmin, xmax, ymin, ymax, key
c
c     Bad key:
c               - positive: mask pixels within region 
c               - negative: mask pixels outside region
c
c     Unlimited number of entries
c


      open (23,file=badb,status='old',err=115)

 114  continue

      read (23,*,end=115) x1,x2,y1,y2,ibad


      
      if (ibad.eq.0) then
      write (*,*)
      write (*,*) 'Incorrect setup of bad pixels, rectangular regions.'
      write (*,*) 'Option inside/outside rectangle cannot be zero.'
      write (*,*) 'Aborting.'
      write (*,*)
      write (*,*)
      stop
      endif


      if (x1.gt.x2) then
      write (*,*)
      write (*,*) 'Mask of bad pixels, rectangular region: bad xmin, xma
     ?x range. Exiting.'
      write (*,*)
      stop
      endif


      if (y1.gt.y2) then
      write (*,*)
      write (*,*) 'Mask of bad pixels, rectangular region: bad ymin, yma
     ?x range. Exiting.'
      write (*,*)
      stop
      endif


      if (ibad.gt.0) then
      
      ix1=x1
      ix2=x2
      iy1=y1
      iy2=y2

      if (ix1.lt.1) ix1=1
      if (iy1.lt.1) iy1=1
      if (ix2.gt.nx) ix2=nx
      if (iy2.gt.ny) iy2=ny

      do ii=iy1,iy2
      do jj=ix1,ix2
      imagem(jj,ii)=-20
      enddo
      enddo

      endif

      
      if (ibad.lt.0) then

      do      ii=1,ny
      do 1114 jj=1,nx
      if(jj.ge.x1.and.jj.le.x2.and.ii.ge.y1.and.ii.le.y2) go to 1114
      imagem(jj,ii)=-20
 1114 continue
      enddo

      endif


      
      go to 114


 115  close (23)



c
c     Bad pixels, rectangular regions, individual images
c
c     Free format: xmin, xmax, ymin, ymax, key
c
c     Bad key:
c               - positive: mask pixels within region 
c               - negative: mask pixels outside region
c
c     Unlimited number of entries
c


      open (23,file=ibadb,status='old',err=117)

 116  continue

      read (23,*,end=117) x1,x2,y1,y2,ibad


      
      if (ibad.eq.0) then
      write (*,*)
      write (*,*) 'Incorrect setup of bad pixels, rectangular regions.'
      write (*,*) 'Option inside/outside rectangle cannot be zero.'
      write (*,*) 'Aborting.'
      write (*,*)
      write (*,*)
      stop
      endif


      if (x1.gt.x2) then
      write (*,*)
      write (*,*) 'Mask of bad pixels, rectangular region: bad xmin, xma
     ?x range. Exiting.'
      write (*,*)
      stop
      endif


      if (y1.gt.y2) then
      write (*,*)
      write (*,*) 'Mask of bad pixels, rectangular region: bad ymin, yma
     ?x range. Exiting.'
      write (*,*)
      stop
      endif


      if (ibad.gt.0) then
      
      ix1=x1
      ix2=x2
      iy1=y1
      iy2=y2

      if (ix1.lt.1) ix1=1
      if (iy1.lt.1) iy1=1
      if (ix2.gt.nx) ix2=nx
      if (iy2.gt.ny) iy2=ny

      do ii=iy1,iy2
      do jj=ix1,ix2
      imagem(jj,ii)=-20
      enddo
      enddo

      endif

      
      if (ibad.lt.0) then

      do      ii=1,ny
      do 1116 jj=1,nx
      if(jj.ge.x1.and.jj.le.x2.and.ii.ge.y1.and.ii.le.y2) go to 1116
      imagem(jj,ii)=-20
 1116 continue
      enddo

      endif


      
      go to 116


 117  close (23)



c
c     Bad pixels, bad range of ADU counts, all images
c
c     Free format: ADU_min, ADU_max, key
c
c     Bad key:
c               - positive: mask pixels within region 
c               - negative: mask pixels outside region
c
c     Unlimited number of entries
c

   

      open (23,file=bada,status='old',err=119)
      


 118  continue

      
      read (23,*,end=119) x1,x2,ibad

     
      if (x1.gt.x2) then
      write (*,*)
      write (*,*) 'Mask of bad pixels, ADU counting. Invalid limits. Exi
     ?ting.'
      write (*,*)
      stop
      endif

      
      if (ibad.eq.0) then
      write (*,*)
      write (*,*) 'Incorrect setup of bad pixels, ADU counting.'
      write (*,*) 'Option inside/outside ADU limits cannot be zero.'
      write (*,*) 'Aborting.'
      write (*,*)
      write (*,*)
      stop
      endif



      if (ibad.gt.0) then
      
      do ii=1,ny
      do jj=1,nx
      if (pixmat(jj,ii).ge.x1 .and. pixmat(jj,ii).le.x2)
     ? imagem(jj,ii)=-20
      enddo
      enddo

      endif


      
      if (ibad.lt.0) then

      do ii=1,ny
      do jj=1,nx
      if (pixmat(jj,ii).lt.x1 .or. pixmat(jj,ii).gt.x2)
     ? imagem(jj,ii)=-20
      enddo
      enddo

      endif

      

      go to 118



 119  close (23)





c
c     Bad pixels, bad range of ADU counts, individual images
c
c     Free format: ADU_min, ADU_max, key
c
c     Bad key:
c               - positive: mask pixels within region 
c               - negative: mask pixels outside region
c
c     Unlimited number of entries
c


      open (23,file=ibada,status='old',err=121)

 120  continue

      read (23,*,end=121) x1,x2,ibad

      if (x1.gt.x2) then
      write (*,*)
      write (*,*) 'Mask of bad pixels, ADU counting. Invalid limits. Exi
     ?ting.'
      write (*,*)
      stop
      endif

      
      if (ibad.eq.0) then
      write (*,*)
      write (*,*) 'Incorrect setup of bad pixels, ADU counting.'
      write (*,*) 'Option inside/outside ADU limits cannot be zero.'
      write (*,*) 'Aborting.'
      write (*,*)
      write (*,*)
      stop
      endif



      if (ibad.gt.0) then
      
      do ii=1,ny
      do jj=1,nx
      if (pixmat(jj,ii).ge.x1 .and. pixmat(jj,ii).le.x2)
     ? imagem(jj,ii)=-20
      enddo
      enddo

      endif


      
      if (ibad.lt.0) then

      do ii=1,ny
      do jj=1,nx
      if (pixmat(jj,ii).lt.x1 .or. pixmat(jj,ii).gt.x2)
     ? imagem(jj,ii)=-20
      enddo
      enddo

      endif

      

      go to 120


 121  close (23)


      

c
c     Adds a constant to make minimum count equal to 100
c     (make all image counts positive)
c

      vmin=+1.d14

      do k=1,ny
      do j=1,nx

      if (imagem(j,k).ge.0) then
      if (pixmat(j,k).lt.vmin) vmin=pixmat(j,k)
      endif

      enddo
      enddo


      do k=1,ny
      do j=1,nx

      if (imagem(j,k).ge.0) then
      pixmat(j,k)=pixmat(j,k)-vmin+100.d0
      else
      pixmat(j,k)=-100.d0
      endif

      enddo
      enddo

      return
      end




c
c
c     Subrotine refits
c
c
c     Reads integer or floating point FITS images
c
c
c      Last update:  M. Assafin  10/Dec/2016
c
c


      subroutine refits (idimx,idimy,pixmat,infits,nx,ny,nheads,ichobj,
     ?ipflag,bscale,bzero,kswap,iswap,nswap,bitpix,mfdp,jfdp,jjfdp,nfdp,
     ?adx,ady,kadx,kady,crpx,crpy,cdx,cdy,kcrpx,kcrpy,kcdx,kcdy)



      implicit real*8 (a-h,o-z)
      parameter (ihead=100)

      integer*2 bitpix

      real*4 pixmat
      integer*2 iwork2
      integer*4 iwork4
      real*4 work4
      integer*8 iwork8
      real*8 work8

      integer*1 swork,iby8


      dimension iwork2(1440),swork(2880),iby8(8)
      dimension work4(720),iwork4(720)
      dimension work8(360),iwork8(360)

      dimension pixmat(idimx,idimy)


      dimension adx(jfdp),ady(jfdp),cdx(2),cdy(2)


      character*150 infits
      character*69 ichobj
      character*9  naxisx,naxisy,bitpx,ibscal,ibzero
      character*1  ler(2880),ibr
      character*9 iend,kend
      character*50 erro
      character*20 imaux
      character*4 jmaux
      character*9 sista
      character*29 systa

      character*8 kadx(jjfdp),kady(jjfdp),kcrpx,kcrpy,kcdx(2),kcdy(2)

      character*2880 header
      character*(ihead*2880) head

c

      data naxisx /'NAXIS1  ='/
      data naxisy /'NAXIS2  ='/
      data bitpx  /'BITPIX  ='/
      data ibscal /'BSCALE  ='/
      data ibzero /'BZERO   ='/
      data iend   /'END      '/
      data ibr    /' '/

c

      nbytes=2880

      if=1

c
c     Opens FITS file
c

      

      open(if,file=infits,access='direct',form='unformatted',recl=2880)


c
c     Opens auxiliary FITS file
c

      sista='rm -f -r '

      imaux=''

      imaux(1:16)='PRAIA_refits.aux'


      do 1 i=1,9999

      write (jmaux,'(i4.4)') i

      imaux(17:20)=jmaux(1:4)

      open(99,file=imaux,access='direct',form='unformatted',
     ?recl=2880,status='old',err=2)
      close (99)
 1    continue

 2    close (99)

      open(99,file=imaux,access='direct',form='unformatted',
     ?recl=2880)

      systa=sista//imaux

c
c     How many headers there are?
c


      iendh=1

      do 20 i=1,ihead

      header='' 

c     read (1,rec=i,err=32) header
      read (1,rec=i,err=30) header

      head(2880*(i-1)+1:2880*i)=header(1:2880)
 

      do k = 1,2880,80
      if (header(k:k+3).eq.'END ') iendh=i
      enddo

 
c
 20   continue

c

 30   nheads=iendh

c

      if (nheads.gt.ihead) then
      write (*,31) ihead
 31   format('Header size exceeded. More than ',i5,' pages. Exiting.')
      ierro=1
      close (1)
      close (99)
      return
      endif 

c
      go to 34

c

 32   continue

      write (*,33)
 33   format('Reached end of fits file. Exiting.')
      ierro=1
      close (1)
      close (99)
      return


c


 34   continue

c     write (*,*)
c     write (*,*) 'nheads ',nheads
c     stop



c
c     Field Distortion Pattern
c
c     Modes 3, 4 (DES): 3rd degree (x,y) polynomial coefficients
c


      if (mfdp.eq.3 .or. mfdp.eq.4) then

      do j=1,nfdp

      do ii=1,nheads

      header='' 
      read(1,rec=ii) header

      do i=1,2880,80

c
c     x nth-coefficient
c

      if (header(i:i+7).eq.kadx(j)) then

      do k=i+9,i+79
      if (header(k:k).ne.' ') go to 100
      enddo

 100  k1=k

      do k=k1+1,i+79
      if (header(k:k).eq.' ') go to 101
      enddo

 101  k2=k-1

      read (header(k1:k2),*) adx(j)

      endif



c
c     y nth-coefficient
c

      if (header(i:i+7).eq.kady(j)) then

      do k=i+9,i+79
      if (header(k:k).ne.' ') go to 102
      enddo

 102  k1=k

      do k=k1+1,i+79
      if (header(k:k).eq.' ') go to 103
      enddo

 103  k2=k-1

      read (header(k1:k2),*) ady(j)

      endif


      enddo

      enddo

      enddo

      endif




c
c     Mode 4 (DES, DECAM): 
c

      if (mfdp.eq.4 .and. nfdp.gt.3) then



c
c     CRPIXi values for DES
c


      do ii=1,nheads

      header='' 
      read(1,rec=ii) header


      do i=1,2880,80


      if (header(i:i+7).eq.kcrpx) then

      do k=i+9,i+79
      if (header(k:k).ne.' ') go to 104
      enddo

 104  k1=k

      do k=k1+1,i+79
      if (header(k:k).eq.' ') go to 105
      enddo

 105  k2=k-1

      read (header(k1:k2),*) crpx

      endif

c

      if (header(i:i+7).eq.kcrpy) then

      do k=i+9,i+79
      if (header(k:k).ne.' ') go to 106
      enddo

 106  k1=k

      do k=k1+1,i+79
      if (header(k:k).eq.' ') go to 107
      enddo

 107  k2=k-1

      read (header(k1:k2),*) crpy

      endif


      enddo

      enddo




c
c     CD_ji linear matrix transformation
c


      do j=1,2

      do ii=1,nheads

      header='' 
      read(1,rec=ii) header

      do i=1,2880,80



      if (header(i:i+7).eq.kcdx(j)) then

      do k=i+9,i+79
      if (header(k:k).ne.' ') go to 108
      enddo

 108  k1=k

      do k=k1+1,i+79
      if (header(k:k).eq.' ') go to 109
      enddo

 109  k2=k-1

      read (header(k1:k2),*) cdx(j)

      endif

c

      if (header(i:i+7).eq.kcdy(j)) then

      do k=i+9,i+79
      if (header(k:k).ne.' ') go to 110
      enddo

 110  k1=k

      do k=k1+1,i+79
      if (header(k:k).eq.' ') go to 111
      enddo

 111  k2=k-1

      read (header(k1:k2),*) cdy(j)

      endif


      enddo
 
      enddo
 
      enddo



      endif





c
c     Debug
c
c
c     do i=1,nfdp
c     write (*,*) 'coef, x = ',kadx(i),adx(i)
c     enddo
c     write (*,*)
c     do i=1,nfdp
c     write (*,*) 'coef, y = ',kady(i),ady(i)
c     enddo
c     stop
c
c
c     write (*,*) 'crpx = ',crpx
c     write (*,*) 'crpy = ',crpy
c
c     do i=1,2
c     write (*,*) 'cdx, cdy = ',cdx(i),cdy(i)
c     enddo
c
c     stop
c


c
c     X matrix dimension
c

      do i=1,nheads

      read(1,rec=i) ler


      call find (ler,naxisx,key,dnx)

      if (key.gt.0) nx=dnx


      enddo



c
c     Y matrix dimension
c

      do i=1,nheads

      read(1,rec=i) ler

      call find (ler,naxisy,key,dny)

      if (key.gt.0) ny=dny

      enddo



c
c     Determines BITPIX
c

      if (bitpix.eq.-99) then

c     bitpix=16

      do i=1,nheads

      read(1,rec=i) ler

      call find (ler,bitpx,key,bpix)

      if (key.gt.0) bitpix=bpix

      enddo

      endif


      i=bitpix
      if (i.lt.0) i=-i

      
      ibytes=i/8+0.1
      kwork=nbytes/ibytes+0.1


c
c     Determines BSCALE
c

      if (ipflag.ne.1) then

      bscale=1.d0

      do i=1,nheads

      read(1,rec=i) ler

      call find (ler,ibscal,key,bsc)

      if (key.gt.0) bscale=bsc

      enddo

      endif


c
c     Determines BZERO
c


      if (ipflag.ne.1) then

      bzero=0.d0

      do i=1,nheads

      read(1,rec=i) ler

      call find (ler,ibzero,key,bzr)

      if (key.gt.0) bzero=bzr

      enddo

      endif


c
c     Reads pixel matrix
c


c
c     Checks byte-swap (Littleendian ou Bigendian)
c
c     kswap : user-defined key
c
c          kswap = 0 automatic determination of byte-swap
c          kswap = 1 no byte-swap
c          kswap = 2 byte-swap
c
c     Automatic byte-swap determination (kswap=0):
c
c     iswap=1  no byte-swap
c     iswap=2  byte-swap
c
c


      if (kswap.ne.0) then

      iswap=kswap

      go to 50

      endif


      irec=nheads+((nx*ny)/2.d0)*ibytes/nbytes 

c

      if (bitpix.gt.0) then

      if (ibytes.eq.2) read (1,rec=irec) iwork2
      if (ibytes.eq.4) read (1,rec=irec) iwork4
      if (ibytes.eq.8) read (1,rec=irec) iwork8

      else

      if (ibytes.eq.4) read (1,rec=irec) work4
      if (ibytes.eq.8) read (1,rec=irec) work8

      endif     


c
c     Average absolute value without swap
c

      c1=0.d0


      if (bitpix.gt.0) then

      do i=1,kwork
      if (ibytes.eq.2) c=bscale*iwork2(i)+bzero
      if (ibytes.eq.4) c=bscale*iwork4(i)+bzero
      if (ibytes.eq.8) c=bscale*iwork8(i)+bzero
      c1=c1+dabs(c)
      enddo

      c1=c1/kwork

      else

      do i=1,kwork
      if (ibytes.eq.4) c=bscale*work4(i)+bzero
      if (ibytes.eq.8) c=bscale*work8(i)+bzero
      c1=c1+dabs(c)
      enddo

      c1=c1/kwork

      endif



c
c     Test it with swap
c


      call swap (if,bitpix,ibytes,nbytes,irec,iwork2,iwork4,iwork8,
     ?work4,work8,swork)




c
c     Average absolute value with swap
c

      c2=0.d0


      if (bitpix.gt.0) then

      do i=1,kwork
      if (ibytes.eq.2) c=bscale*iwork2(i)+bzero
      if (ibytes.eq.4) c=bscale*iwork4(i)+bzero
      if (ibytes.eq.8) c=bscale*iwork8(i)+bzero
      c2=c2+dabs(c)
      enddo

      c2=c2/kwork

      else

      do i=1,kwork
      if (ibytes.eq.4) c=bscale*work4(i)+bzero
      if (ibytes.eq.8) c=bscale*work8(i)+bzero
      c2=c2+dabs(c)
      enddo

      c2=c2/kwork

      endif

c
c     Defines swap
c

      erro=''

      write (erro,*) c1

      do i=1,50
      if (ichar(erro(i:i)).ge.48 .and. ichar(erro(i:i)).le.57) go to 35
      enddo

      c1=1.d14

 35   continue

      if (c1.lt.1.d-10) c1=1.d14 

c

      erro=''
     
      write (erro,*) c2

      do i=1,50
      if (ichar(erro(i:i)).ge.48 .and. ichar(erro(i:i)).le.57) go to 40
      enddo

      c2=1.d14

 40   continue    

      if (c2.lt.1.d-10) c2=1.d14 


c

      if (c2.lt.c1) then
      iswap=2
      else
      iswap=1
      endif     


c     write (*,*) 'c1 c2 ',c1, c2

c
c     Reads pixel matrix
c

 50   continue


c

      block=nx*ny*ibytes 
      block=block/nbytes 
      nblock=block
      iresto=(block-nblock)*nbytes
      iresto=iresto/ibytes+0.2

c     write (*,*) 'nx,ny,ibytes,nbytes,block,nblock,iresto = ',nx,ny,
c    ?ibytes,nbytes,block,nblock,iresto

      j=0
      i=1

      do m=1,nblock

      irec=m+nheads

c


      if (iswap.eq.1) then

      if (bitpix.gt.0) then

      if (ibytes.eq.2) read (1,rec=irec) iwork2
      if (ibytes.eq.4) read (1,rec=irec) iwork4
      if (ibytes.eq.8) read (1,rec=irec) iwork8

      else

      if (ibytes.eq.4) read (1,rec=irec) work4
      if (ibytes.eq.8) read (1,rec=irec) work8


      endif

      else

      call swap (if,bitpix,ibytes,nbytes,irec,iwork2,iwork4,iwork8,
     ?work4,work8,swork)

      endif

c

      if (bitpix.gt.0) then

      do mm=1,kwork

      j=j+1
      if (j.gt.nx) then
      j=1
      i=i+1
      endif

      if (ibytes.eq.2) pixmat(j,i)=bscale*iwork2(mm)+bzero
      if (ibytes.eq.4) pixmat(j,i)=bscale*iwork4(mm)+bzero
      if (ibytes.eq.8) pixmat(j,i)=bscale*iwork8(mm)+bzero

      enddo

      else

      do mm=1,kwork

      j=j+1
      if (j.gt.nx) then
      j=1
      i=i+1
      endif


      if (ibytes.eq.4) pixmat(j,i)=bscale*work4(mm)+bzero
      if (ibytes.eq.8) pixmat(j,i)=bscale*work8(mm)+bzero



      enddo      
 

      endif

      enddo

c
c     Last block piece of the matrix (if existent)
c


      if (iresto.ne.0) THEN



      irec=irec+1

      if (iswap.eq.1) then

      if (bitpix.gt.0) then

      if (ibytes.eq.2) read (1,rec=irec) (iwork2(m),m=1,iresto)
      if (ibytes.eq.4) read (1,rec=irec) (iwork4(m),m=1,iresto)
      if (ibytes.eq.8) read (1,rec=irec) (iwork8(m),m=1,iresto)

      else

      if (ibytes.eq.4) read (1,rec=irec) (work4(m),m=1,iresto)
      if (ibytes.eq.8) read (1,rec=irec) (work8(m),m=1,iresto)
     
      endif

      else

      nbytes=iresto*ibytes

      call swap (if,bitpix,ibytes,nbytes,irec,iwork2,iwork4,iwork8,
     ?work4,work8,swork)

      endif

c
      if (bitpix.gt.0) then

      do mm=1,iresto

      j=j+1
      if (j.gt.nx) then
      j=1
      i=i+1
      endif

      if (ibytes.eq.2) pixmat(j,i)=bscale*iwork2(mm)+bzero
      if (ibytes.eq.4) pixmat(j,i)=bscale*iwork4(mm)+bzero
      if (ibytes.eq.8) pixmat(j,i)=bscale*iwork8(mm)+bzero

      enddo      

      else

      do mm=1,iresto

      j=j+1
      if (j.gt.nx) then
      j=1
      i=i+1
      endif

      if (ibytes.eq.4) pixmat(j,i)=bscale*work4(mm)+bzero
      if (ibytes.eq.8) pixmat(j,i)=bscale*work8(mm)+bzero

      enddo      

      endif
c

      ENDIF

c

      close (1)
      close (99)


c
c     Sample a pixel to check image reading configurations
c


      if (nswap.eq.1) then

      mmm1=nx/2
      mmm2=ny/2

      write (*,*)
      write (*,*)


      write (*,*) 'Pixel sample to check for image reading configuration
     ?s:'
      write (*,*)

      write (*,60) mmm1,mmm2,pixmat(mmm1,mmm2)
 60   format(' Image (',i5.5,',',i5.5') = ',f18.6)


      write (*,*)
      write (*,*)

      endif



c
c     Debug
c
c
c     read (*,*) j,i
c     j=60
c     i=60 
c     write (*,*) 'ix iy = ',j,i
c     write (*,*) 'pixmat = ',pixmat(j,i)
c     write (*,*) 'swap ',iswap
c     write (*,*) 'c1 c2 = ',c1,c2
c     write (*,*) 'nheads = ',nheads
c     write (*,*) 'bitpix = ',bitpix
c     write (*,*) 'bscale = ',bscale
c     write (*,*) 'bzero  = ',bzero 
c     stop
 


      call system (systa)

c

      return
      end




c
c
c     subroutine find
c
c
c     Finds the numerical value associated to the key word
c     of the FITS header
c
c     ler     = header extraction
c     word    = header word
c     key     = +1 found it
c             = -1 didn't find it
c     valor   = numerical value found
c
c
c
c      Last update:  M. Assafin  27/Aug/2009
c



      subroutine find (ler,word,key,valor)

      IMPLICIT REAL *8 (A-H,O-Z)
      

      character*1  ler(2880),iplic,ibr,ibar
      character*9  word,kend
      character*1 ivalor(71)




      data iplic /"'"/
      data ibar  /"/"/
      data ibr   /' '/

c

      key=-1
      valor=-1.d14

      icol=1
      id=71

c

      do j=0,35

      do i=1,9
      kend(i:i)=ler(j*80+i)
      enddo

      if (kend.eq.word) then
      key=+1
      go to 20
      endif

      enddo

      go to 50

c

 20   j=j*80
      do i=10,80
      if (ler(j+i).ne.ibr .and. ler(j+i).ne.iplic) go to 30
      enddo
 30   i1=i
      do i=i1+1,80
      if (ler(j+i).eq.ibr .or. ler(j+i).eq.ibar) go to 40
      enddo

 40   i2=i-1

c
      do i=1,id
      ivalor(i)=ibr
      enddo
c

      n=0

      do i=j+i1,j+i2
      n=n+1
      ivalor(n)=ler(i)
      enddo

c

      call chanum (icol,id,ivalor,valor)


 50   continue
      return

      end




c
c     subroutine chanum
c
c
c     Takes a character string with numbers and extracts the number
c     associated to a column number
c
c     string  = contains the complete string
c     palavra = auxiliary variable containing the complete string
c     valor = extracted number for the given column number
c     
c
c      Last update:  M. Assafin  27/Aug/2009
c
c
c
      subroutine chanum (icol,id,string,valor)

      implicit real *8 (a-h,o-z)

      integer*8 n

      dimension ni(id+2),nf(id+2)

      character*1 string(id),palavra(id+2),ibra

c

      ibra=' '

c

      do i=1,id+2
      palavra(i)=ibra
      enddo

      do i=1,id
      palavra(i+1)=string(i)
      enddo

c
c     Finds columns separated by blank spaces
c

      do i=1,id
      ni(id)=0
      nf(id)=0
      enddo

      ki=0
      kf=0

c
c     The beginning of the number
c

      do i=2,id+2

      if (palavra(i-1).eq.ibra .and. palavra(i).ne.ibra) then
      ki=ki+1
      ni(ki)=i
      endif

      enddo

c
c     The ending of the number
c

      do i=2,id+2

      if (palavra(i-1).ne.ibra .and. palavra(i).eq.ibra) then
      kf=kf+1
      nf(kf)=i-1
      endif
      
      enddo

c
c     Number is positive or negative?
c

      isig=+1

      i=ni(icol)
      if (palavra(i).eq.'-') isig=-1
  

c
c     Number in "E" ou "D" notation?
c

      iep=0
      ie=0
      isige=+1


      do i=ni(icol),nf(icol)
      if (palavra(i).eq.'e'.or.palavra(i).eq.'E'.or.palavra(i).eq.'d'
     ?.or.palavra(i).eq.'D') ie=i
      enddo

      if (ie.ne.0) then

      iee=ie

      if (palavra(ie+1).eq.'-') then
      isige=-1
      endif

      if (palavra(ie+1).eq.'-'.or.palavra(ie+1).eq.'+') then
      ie=ie+2
      else
      ie=ie+1
      endif


      iep=0
      j=0
      k=nf(icol)-ie+1
      do i=ie,nf(icol)
      icomp=ichar(palavra(i))
      j=j+1
      iep=iep+(icomp-48)*10.d0**(k-j)
      enddo

      nf(icol)=iee-1

      endif

      expo=10.d0**(isige*iep)


c
c     Where is the decimal of the number (if it is not an integer number)
c

      m=0
      do i=nf(icol),ni(icol),-1
      if (palavra(i).eq.'.') m=i-nf(icol)
      enddo

c
c     Significant number figures
c

      k=0
      do i=ni(icol),nf(icol)
      if (palavra(i).ne.'.' .and. palavra(i).ne.'+' .and. palavra(i).
     ?ne.'-') k=k+1
      enddo

      n=0
      j=0
      do i=ni(icol),nf(icol)
      icomp=ichar(palavra(i))
      if (icomp.ge.48 .and. icomp.le.57) then
      j=j+1
      n=n+(icomp-48)*10.d0**(k-j)
      endif
      enddo


      valor=expo*isig*n*10.d0**m

      return
      end




c
c
c     subroutine swap
c
c
c     Swap bytes of FITS image.
c
c     Image can be integer ou floating point.
c
c 
c     Last update: M. Assafin 15/Aug/2015
c
c


      subroutine swap(if,bitpix,ibytes,nbytes,irec,iwork2,iwork4,iwork8,
     ?work4,work8,swork)


      IMPLICIT REAL *8 (A-H,O-Z)



      dimension iwork2(1440),swork(2880),iby8(8)
      dimension work4(720),iwork4(720)
      dimension work8(360),iwork8(360)

      integer*2 bitpix

c     real*4 pixmat
      integer*2 iwork2
      integer*4 iwork4
      real*4 work4
      integer*8 iwork8
      real*8 work8

      integer*1 swork,iby8

c

      read (if,rec=irec) swork


      do k=ibytes,nbytes,ibytes

      do m=1,ibytes
      iby8(m)=swork(k-m+1)
      enddo

      do m=1,ibytes
      swork(k-ibytes+m)=iby8(m)
      enddo

      enddo

      write (99,rec=1) swork

      if (bitpix.gt.0) then

      if (ibytes.eq.2) read (99,rec=1) iwork2
      if (ibytes.eq.4) read (99,rec=1) iwork4
      if (ibytes.eq.8) read (99,rec=1) iwork8

      else

      if (ibytes.eq.4) read (99,rec=1) work4
      if (ibytes.eq.8) read (99,rec=1) work8

      endif     

c

      return
      end





c
c
c      subroutine psf
c
c
c      Fits 2D PSF models to the detected rounded-shaped objects.
c      The pixels are fitted with a model choosen by the user,
c      according to the followinf alternatives:
c
c      - key 2: 2D Circular Gaussian model
c
c      - key 3: 2D Elliptical Gaussian model
c
c      - key 4: 2D Circular Lorentz model with alpha and beta exponents
c
c      - key 5: 2D Elliptical Lorentz model with alpha and beta exponents
c
c
c      Routines are based in the Marquardt non-linear least squares
c      procedures by Bevington (1969), extended to 2 dimensions.
c
c
c      Variables:
c
c
c
c      fac     - sigma factor for elimination of pixel outliers in PSF fitting
c
c      dlimit  - sigma convergence in ADU PSF fitting
c
c      plimit  - (x,y) convergence in PSF fitting
c
c      kcontx  - maximum number of L.S. iterations in non-LS Marquardt method
c
c      iwr     - weight key: (1) weight pixels; (2) don't weight pixels;
c                here, only the statistical weighting is used (w=1/error^2,
c                error = sqrt(pixel_count), that is, w=1/pixel_count)
c
c      raio    - aperture radius with valid pixels to be fitted
c      bx,by   - fixed center of aperture radius with valid pixels to be fitted
c
c      ix1,ix2 - fixed x pixel limits containing the aperture
c      iy1,iy2 - fixed y pixel limits containing the aperture
c
c      sigdex  - output x center error in pixel units
c      sigdey  - output y center error in pixel units
c
c      sigx    - input  => sigma from imagem moments (pixel units)
c                output => sigma of PSF model (pixel units)
c
c      alturx  - highness of PSF model
c
c      fundox  - fixed sky background taken from BOIA during object identification
c
c      u20,u02,u11 - object image's second moments
c
c      pc      - total flux inside the aperture above the sky background (clean flux)
c
c      fs      - standard deviation of the sky background flux of the
c                sky background ring
c
c      nfs     - number of points used in the computation of the standard deviation
c                of the sky background flux of the sky background ring
c
c      tet     - orientation angle computed from object image's second moments
c                (radians)
c
c      seg1    - input  semi-major axis computed from object image's second moments
c                (pixels)
c      eg1     - output semi-major axis computed from PSF fit (pixels)
c
c      seg2    - input  semi-minor axis computed from object image's second moments
c                (pixels)
c      eg2     - output semi-minor axis computed from PSF fit (pixels)
c
c      exc     - output excentricity
c
c
c      resx    - standard deviation (sigma or error of unit weight)
c                of PSF fit in ADU units
c
c      cut     - percentage tolerance below which the floor PSF is considered the
c                true PSF of the object (that is, there is no object to fit a PSF to)
c
c
c
c      Last modification: M. Assafin 01/Mar/2022
c 
c 

      subroutine psf(idimx,idimy,idiobs,icofsp,pixmat,imagem,fac,dlimit,
     ?plimit,kcontx,nx,ny,ix1,ix2,iy1,iy2,raio,bx,by,sigdex,sigdey,sigx,
     ?alturx,fundox,pc,fs,nfs,u20,u02,u11,seg1,seg2,tet,exc,iwr,mkey,
     ?ierro,resx,sv,ior,xest,yest,xp,coefx,u,v,w,wsig,cut,qua)

      implicit real*8 (a-h,o-z)

      real*4 pixmat(idimx,idimy),weight(idimx,idimy)

      integer*2 imagem(idimx,idimy)

      dimension xsigma(icofsp),param(icofsp),sv(idiobs),ior(idiobs)

      dimension coefx(icofsp),wsig(idiobs),xest(idiobs),yest(idiobs),
     ?xp(idiobs),u(idiobs),v(idiobs),w(idiobs)


c
c     Initial values
c

      pi=3.141592653589793238462643d0

      ifora=10

      ierro=0


c
c     PSF model keys
c

      key=mkey

      if (mkey.eq.5) key=4


c


      xref=bx
      yref=by

      ixref=xref
      iyref=yref

      eg1=seg1
      eg2=seg2

c
c     Errors of parameters excluding sky background
c

      do i=1,icofsp
      xsigma(i)=0.d0
      enddo

c
c     Weights
c

      if (iwr.eq.1) then

      do i=iy1,iy2
      do j=ix1,ix2
      if (pixmat(j,i).gt.0.d0) then
      weight(j,i)=1.d0/pixmat(j,i)
      else
      weight(j,i)=0.d0
      endif
      enddo
      enddo

      else

      do i=iy1,iy2
      do j=ix1,ix2
      weight(j,i)=1.d0
      enddo
      enddo


      endif




c
c     Floor PSF
c
c
c     Fits a plane to the furnished region.
c
c     Usefull to certificate that a true object
c     was actually found, by comparing the sigma
c     from the Floor PSF fit with the sigma from
c     the PSF fit.
c



      do i=1,icofsp
      coefx(i)=0.d0
      enddo


      index=1

      nterms=3


      n=0

      do 2 i=iy1,iy2

      call circul (raio,xref,yref,ixref,i,ichave)
      if (ichave.lt.0) go to 2

      do 1 j=ix1,ix2

      if (pixmat(j,i).lt.0.d0) go to 1
c     if (imagem(j,i).eq.-20) go to 1

      call circul (raio,xref,yref,j,i,ichave)

      if (ichave.lt.0) go to 1

      n=n+1

      xest(n)=j
      yest(n)=i

      xp(n)=pixmat(j,i)-fundox

c     wsig(n)=dsqrt(1.d0/weight(j,i)**2)


c     wsig(n)=dsqrt(pixmat(j,i))
c     wsig(n)=dsqrt(1.d0/weight(j,i))

      wsig(n)=1.d0



 1    continue

 2    continue


      if (n.lt.3) then
      ierro=1
      return
      endif


      call svdfit (xest,yest,xp,wsig,n,coefx,nterms,u,v,w,idiobs,
     ?icofsp,chisqx,idiobs,icofsp,index)



      xm=0.d0
      xs=0.d0

      do i=1,n

      x=fpol(icofsp,index,xest(i),yest(i),coefx,nterms,i,n)

      xx=x-xp(i)

      xm=xm+xx
      xs=xs+xx**2

      enddo


c
c     Finally, the sigma from the Floor PSF fit
c 

      call desvio (n,xm,xs)

      nfl=n



c
c     Initial parameters for non-linear L.S. fit
c     of the PSF model (2-5)
c



c
c     Center and highness
c


      j1=ixref-1
      j2=ixref+1

      i1=iyref-1
      i2=iyref+1

      if (i1.lt.1) i1=1
      if (j1.lt.1) j1=1

      if (i2.gt.ny) i2=ny
      if (j2.gt.nx) j2=nx

      cmax=-1.d14

      do ii=i1,i2
      do jj=j1,j2
      if (pixmat(jj,ii).gt.0.d0.and.pixmat(jj,ii).gt.cmax) then
c     if (imagem(jj,ii).ne.-20.and.pixmat(jj,ii).gt.cmax) then
      cmax=pixmat(jj,ii)
      alturx=pixmat(jj,ii)-fundox
      ixc=jj
      iyc=ii
      endif
      enddo
      enddo

      if (cmax.eq.-1.d14) then
      ierro=1
      return
      endif

c
c     Eventually the pixels in the center are saturated or with
c     low counts due to saturation artifacts. In this case, try
c     the highness estimation based on the total volume under
c     a 2D Circular Gaussian distribution which relates its
c     highness with its (estimated) sigma:
c
c
c     volume (approx. total flux inside region) = 2.pi.highness.sigma^2 
c
c


      cmax=pc/(2.d0*pi*sigx**2)

      if (cmax.gt.alturx) alturx=cmax


c
c     All psf models
c

      param(1)=ixc
      param(2)=iyc


c
c     2D Circular Gaussian
c

      if (key.eq.2) then

c
c
c     For statistical Gaussian, the sigma of the Gaussian is
c     approximatelly equal to that of the sigma from object
c     image's second moments, for apertures with radius as
c     large as those found by the BOIA method (optimal S/R,
c     1 radius = 1 FWHM = aprox. 2.5 sigma_Gauss, confirming
c     Stone, 1989)
c
c


c     param(3)=2.d0*sigx

      param(3)=sigx

      param(4)=alturx

      param(5)=fundox

      xlamda=0.001d0


      endif


c
c     2D Elliptical Gaussian
c

      if (key.eq.3) then


c
c     x^2, xy and y^2 ellipse-related coefficients
c
c     eg1, eg2 are the semi-major and semi-minor axes from
c     object image's second moments
c


      param(3)=(dcos(tet)/eg1)**2+(dsin(tet)/eg2)**2
 
      param(4)=-2.d0*dcos(tet)*dsin(tet)*(1.d0/eg2**2-1.d0/eg1**2)
 
      param(5)=(dcos(tet)/eg2)**2+(dsin(tet)/eg1)**2


      param(6)=alturx

      param(7)=fundox

      xlamda=0.001d0


      endif



c
c     2D Circular Cauchy-Lorentz model with alpha and beta exponents
c
c

      if (key.eq.4) then


c
c     Estimates the Lorentzian sigma (r0).
c
c     The exponents alpha and beta default starting
c     values are:
c
c     alpha = 2.0 (transition between spiky and flat central peak)
c
c     beta  = 1.0 (the Moffat distribution as a transition between
c                  elevated and shallower wing background)
c
c
c     The alpha value is refined after the estimation of r0.
c

      alfa=2.d0

      beta=1.d0

c     beta=2.5d0


      call rab (idimx,idimy,idiobs,pixmat,imagem,sv,ior,nx,ny,raio,
     ?param(1),param(2),alturx,fundox,r0,alfa,beta,ierro,qua)


      if (ierro.eq.1) return


      param(3)=r0

      param(4)=alturx

      param(5)=alfa

      param(6)=beta

      param(7)=fundox

      xlamda=0.001d0


      endif




c
c     2D Elliptical Cauchy-Lorentz model with alpha and beta exponents
c


      if (key.ne.5) go to 12

 5    continue


c
c     Estimates the effective sigma r0 taking a Circular
c     Lorentzian distribution as basis.
c
c     The exponents alpha and beta default starting
c     values are:
c
c     alpha = 2.0 (transition between spiky and flat central peak)
c
c     beta  = 1.0 (the Moffat distribution as a transition between
c                  elevated and shallower wing background)
c
c
c     The alpha value is refined after the estimation of r0.
c


      alfa=2.d0

      beta=1.d0

c     beta=2.5d0


      call rab (idimx,idimy,idiobs,pixmat,imagem,sv,ior,nx,ny,raio,
     ?param(1),param(2),alturx,fundox,r0,alfa,beta,ierro,qua)


      if (ierro.eq.1) return


c
c     Estimates the semi-major and semi-minor axes
c

 10   continue



c
c     Estimates the semi-major and semi-minor axes of the Elliptical
c     Lorentzian from the proportions between the semi-major
c     and semi-minor axes and the equivalent sigma from the
c     object image's second moments, scaled by the r0 obtained
c     from a previous Circular Lorentzian fitting, or by the rab method.
c

      eg1=eg1/sigx
      eg2=eg2/sigx


      eg1=eg1*r0
      eg2=eg2*r0



c
c     x^2, xy and y^2 ellipse-related coefficients
c


      param(3)=(dcos(tet)/eg1)**2+(dsin(tet)/eg2)**2
 
      param(4)=-2.d0*dcos(tet)*dsin(tet)*(1.d0/eg2**2-1.d0/eg1**2)
 
      param(5)=(dcos(tet)/eg2)**2+(dsin(tet)/eg1)**2


      param(6)=alturx

      param(7)=alfa

      param(8)=beta

      param(9)=fundox

      param(1)=bx
      param(2)=by

      xlamda=0.001d0


 12   continue



c
c     Number of terms in PSF model
c     excluding the sky background
c


      if (key.eq.2) ntermx=4
      if (key.eq.3) ntermx=6
      if (key.eq.4) ntermx=6
      if (key.eq.5) ntermx=8


c
c     PSF model fitting
c

      ierro=0

      icont=0

      icontx=1

      ifor=0

      xresid=-1.d14
      residx=0.d0

      xcent=1.d14
      ycent=1.d14


 14   call psffit (idimx,idimy,icofsp,pixmat,imagem,raio,xref,yref,
     ?ixref,ix1,iy1,ix2,iy2,ntermx,param,xsigma,xlamda,residx,icont,
     ?weight,key,ierro)

c     write (*,*) 'icontx,ierro,x,y,x2,xy,y2,h,alfa,beta,fundo = ',
c    ?icontx,ierro,(param(kkk),kkk=1,9)
c
c     write (*,*) 'xref,yref  = ',xref,yref
c     write (*,*) 'icont,ifor = ',icont,ifor
c     write (*,*) 'flamda = ',xlamda


c
c     Checks for errors
c

      if (ierro.eq.1) then
      if (mkey.ne.5) return
      if (mkey.eq.5.and.key.eq.5) return
      ierro=0
      key=5
      go to 5
      endif


      if (residx.lt.0.d0) then
      ierro=1
      if (mkey.ne.5) return
      if (mkey.eq.5.and.key.eq.5) return
      ierro=0
      key=5
      go to 5
      endif

  

c
c     Checks convergence of internal L.S. iterations
c


      residx = dsqrt(residx)
      centx  = param(1)
      centy  = param(2)
      conver = dabs(residx*dlimit)
      diferd = dabs(residx-xresid)
      difpox = dabs(centx-xcent)
      difpoy = dabs(centy-ycent)

      if ((diferd.lt.conver).and.(difpox.lt.plimit).and.(difpoy.lt.
     ?plimit)) go to 150

      xresid = residx
      xcent=centx
      ycent=centy

      icontx = icontx + 1

      if (icontx.gt.kcontx) go to 150

      go to 14

c

  150 continue


c
c     Eliminates pixels with discrepant counts by a factor of the sigma
c     of the PSF fitting.
c
c     The first ifora=10 pixels are eliminated one by one at each
c     iteration, then all pixels above the threshold are eliminated at
c     each iteration until no pixel is above the threshold.
c


      if (icont.le.ntermx+1) go to 160

c

      teto=fac*residx

      difmax=-1.d14

      n=0


c
c     2D Circular Gaussian PSF function
c

      IF (key.eq.2) THEN


      do 32 i=iy1,iy2
      do 22 j=ix1,ix2

      if (imagem(j,i).lt.-9) go to 22

      call circul (raio,xref,yref,j,i,ichave)
      if (ichave.lt.0) go to 22

      difcon=dabs(pixmat(j,i)-psf2(icofsp,j,i,param,ierro))

      if (difcon.gt.teto) then

      if (ifor.gt.ifora) then

      imagem(j,i)=-10
      n=1

      else

      if (difcon.gt.difmax) then
      difmax=difcon
      n=1
      jxx=j
      iyy=i
      endif

      endif

      endif

 22   continue
 32   continue


      ENDIF


c
c     2D Elliptical Gaussian PSF function
c

      IF (key.eq.3) THEN


      do 33 i=iy1,iy2
      do 23 j=ix1,ix2

      if (imagem(j,i).lt.-9) go to 23

      call circul (raio,xref,yref,j,i,ichave)

      if (ichave.lt.0) go to 23

      difcon=dabs(pixmat(j,i)-psf3(icofsp,j,i,param,ierro))

      if (difcon.gt.teto) then

      if (ifor.gt.ifora) then

      imagem(j,i)=-10
      n=1

      else

      if (difcon.gt.difmax) then
      difmax=difcon
      n=1
      jxx=j
      iyy=i
      endif

      endif

      endif

 23   continue
 33   continue


      ENDIF


c
c     2D Circular Cauchy-Lorentz model with
c     alpha and beta exponents
c


      IF (key.eq.4) THEN


      do 34 i=iy1,iy2
      do 24 j=ix1,ix2

      if (imagem(j,i).lt.-9) go to 24

      call circul (raio,xref,yref,j,i,ichave)
      if (ichave.lt.0) go to 24

      difcon=dabs(pixmat(j,i)-psf4(icofsp,j,i,param,ierro))

      if (difcon.gt.teto) then

      if (ifor.gt.ifora) then

      imagem(j,i)=-10
      n=1

      else

      if (difcon.gt.difmax) then
      difmax=difcon
      n=1
      jxx=j
      iyy=i
      endif

      endif

      endif

 24   continue
 34   continue


      ENDIF

c
c     2D Elliptical Cauchy-Lorentz model with
c     alpha and beta exponents
c



      IF (key.eq.5) THEN


      do 35 i=iy1,iy2
      do 25 j=ix1,ix2

      if (imagem(j,i).lt.-9) go to 25

      call circul (raio,xref,yref,j,i,ichave)
      if (ichave.lt.0) go to 25

      difcon=dabs(pixmat(j,i)-psf5(icofsp,j,i,param,ierro))

      if (difcon.gt.teto) then

      if (ifor.gt.ifora) then
 
      imagem(j,i)=-10
      n=1
 
      else

      if (difcon.gt.difmax) then
      difmax=difcon
c     n=n+1
      n=1
      jxx=j
      iyy=i
      endif

      endif

      endif

 25   continue
 35   continue


      ENDIF

c


      if (n.ne.0) then

      if (ifor.le.ifora) then

      imagem(jxx,iyy)=-10

      ifor=ifor+1

      endif

      icontx=1


      xlamda=0.001d0

c     if (key.eq.2) xlamda=0.001d0
c     if (key.eq.3) xlamda=0.001d0
c     if (key.eq.4) xlamda=0.001d0
c     if (key.eq.5) xlamda=0.001d0

      go to 14

      endif


c
c     Convergence reached. PSF fitting stoped.
c
c     Restores eliminated pixels with discrepant counts
c     by a factor of the sigma of the PSF fitting of the
c     previous iterations
c

 160  continue

      do i=iy1,iy2
      do j=ix1,ix2
 
      if (imagem(j,i).eq.-10) imagem(j,i)=0
 
      enddo
      enddo


c
c     Flushes fitted parameters
c


      bx     = param(1)
      by     = param(2)

      resx=dsqrt((residx**2)*(icont-ntermx)/(icont-1.d0))


      if (iwr.eq.2) then

      sigdex = xsigma(1)*residx
      sigdey = xsigma(2)*residx

      else

      sigdex = xsigma(1)
      sigdey = xsigma(2)

      endif



c
c     2D Circular Gaussian
c

      if (key.eq.2) then

      sigx   = param(3)
      alturx = param(4)


      endif


c
c     2D Elliptical Gaussian
c


      if (key.eq.3) then

      alturx=param(6)



c
c     Computes ellipse-related parameters
c

      a=param(3)
      b=param(4)
      c=param(5)


c
c     Computes the orientation angle
c



      sn=c-a-dsqrt((a-c)**2+b**2)
      cs=b


      if (cs.eq.0.d0) then

      if (c.ge.a) then

      tet=0.d0

      else

      tet=pi/2.d0

      endif

      else

      tet=theta(cs,sn)

      endif

      if (tet.gt.pi) tet=tet-pi




c
c     Computes the semi-major and semi-minor axes
c


      eg1=-dsqrt(-2.d0*(b**2-4.d0*a*c)*(a+c+dsqrt((a-c)**2+b**2)))/(b**2
     ?-4.d0*a*c)

      eg2=-dsqrt(-2.d0*(b**2-4.d0*a*c)*(a+c-dsqrt((a-c)**2+b**2)))/(b**2
     ?-4.d0*a*c)



c
c     Computes equivalent sigma (sigma if Gaussian
c     was circular)
c

      sigx=dsqrt(eg1*eg2)



c
c     Computes the excentricity
c


      exc=dsqrt(1.d0-(eg2/eg1)**2)



      endif


c
c     2D Circular Lorentz model with alpha and beta exponents
c


      if (key.eq.4) then


      alturx=param(4)


c
c     For converting from pure Lorentzian sigma to
c     statistical Gaussian sigma, we must apply the factor:
c
c
c     Ro(Gaussian) = Ro(Lorentzian) / sqrt(2*beta*ln(2))
c

      aux=sigx


      sigx=param(3)/dsqrt(2.d0*dabs(param(6))*dlog(2.d0))



c
c     2D Circular Lorentz as input for 2D Elliptical
c     Lorentz model?
c

      if (mkey.eq.5) then

      sigx=aux
      r0=param(3)
      alfa=param(5)
      beta=param(6)

      key=5

      go to 10

      endif


      endif


c
c     2D Elliptical Lorentz model with alpha and beta exponents
c


      if (key.eq.5) then


      alturx=param(6)


c
c     Computes ellipse-related parameters
c


      a=param(3)
      b=param(4)
      c=param(5)



c
c     Compute the angle
c


      sn=c-a-dsqrt((a-c)**2+b**2)
      cs=b


      if (cs.eq.0.d0) then

      if (c.ge.a) then

      tet=0.d0

      else

      tet=pi/2.d0

      endif

      else

      tet=theta(cs,sn)

      endif

      if (tet.gt.pi) tet=tet-pi



c
c     Computes semi-major and semi-minor axes
c


      eg1=-dsqrt(-2.d0*(b**2-4.d0*a*c)*(a+c+dsqrt((a-c)**2+b**2)))/(b**2
     ?-4.d0*a*c)

      eg2=-dsqrt(-2.d0*(b**2-4.d0*a*c)*(a+c-dsqrt((a-c)**2+b**2)))/(b**2
     ?-4.d0*a*c)



c
c     For converting from pure Elliptical Lorentzian semi-axes to
c     statistical Elliptical Gaussian semi-axes, we must apply
c     the factor:
c
c
c     Axes(Gaussian) = Axes(Lorentzian) / sqrt(2*beta*ln(2))
c
c


      eg1=eg1/dsqrt(2.d0*dabs(param(8))*dlog(2.d0))
      eg2=eg2/dsqrt(2.d0*dabs(param(8))*dlog(2.d0))




c
c     The equivalent sigma
c


      sigx=dsqrt(eg1*eg2)


c
c     Computes the excentricity
c

      exc=dsqrt(1.d0-(eg2/eg1)**2)



      endif


c
c     Tests the sigma of the Floor PSF fit against the
c     sigma of the PSF fit.
c
c
c     Using the F-test, checks if the two sigmas are
c     statistically different. If they are statistically
c     equal, cancels the PSF fit of this object.
c
c     The threhsrolds P of probability rejection by the
c     PSF Floor fitting correspond to a X sigma factor
c     rejection criterion such that:
c
c
c     P = 1 - 0.382924922548026   ====>  X = 0.5
c
c     P = 1 - 0.682689492137086   ====>  X = 1.0
c
c     P = 1 - 0.866385597462284   ====>  X = 1.5
c
c     P = 1 - 0.954499736103642   ====>  X = 2.0
c
c     P = 1 - 0.997300203936740   ====>  X = 3.0
c
c     P = 1 - 0.999936657516334   ====>  X = 4.0
c
c     etc
c


      call ftesta (nfl,icont,xs,residx,f,prob)

c     IF (prob.ge.0.d0) THEN

      if (prob.ge.(1.d0-0.382924922548026d0)) then

c     if (prob.ge.(1.d0-0.682689492137086d0)) then
c     if (prob.ge.10.d-2) then
c     if (prob.ge.5.d-2) then
c     if (prob.ge.(1.d0-0.997300203936740d0)) then
c     if (prob.ge.(1.d0-0.999936657516334d0)) then


      ierro=1
      return

      endif

c     ELSE
c
c     ierro=1
c     return
c
c     ENDIF



c     aux=100*(xs-residx)/residx


c     if (aux.le.cut) ierro=1

c     write (*,*) 'residx,xs,ierro = ',residx,xs,ierro
c     write (*,*) 'residx/xs,per = ',residx/xs,100*(xs-residx)/residx
c     write (789,*) '00 ',100*(xs-residx)/residx,residx/xs,residx,xs

c

c     write (2001,*) xs,obxs,bx,by


      return
      end




c
c
c      Subroutine psffit
c
c
c      Performs the non-linear least-squares Marquardt fitting of
c      2-D PSF models:
c
c      - key 2: 2D Circular Gaussian model
c
c      - key 3: 2D Elliptical Gaussian model
c
c      - key 4: 2D Circular Cauchy-Lorentz model
c
c      - key 5: 2D Cauchy-Elliptical Lorentz model
c
c
c      For all fits, the sky background parameter is fixed and is
c      not adjusted. It is previously determined from the best
c      S/N ratio aperture evaluation from the PRAIA BOIA procedure.
c
c
c
c      Last modification: M. Assafin 01/Mar/2022
c
c

      subroutine psffit (idimx,idimy,icofsp,pixmat,imagem,raio,xref,
     ?yref,ixref,nx,ny,mx,my,nterms,a,sigmaa,flamda,chisqr,icont,weight,
     ?key,ierro)

      implicit real *8 (a-h,o-z)

      real*4 pixmat(idimx,idimy),weight(idimx,idimy)
      integer*2 imagem(idimx,idimy)

      dimension a(icofsp),sigmaa(icofsp),b(icofsp),alpha(icofsp,icofsp),
     ?beta(icofsp),deriv(icofsp),array(icofsp,icofsp),c(icofsp)

      character*32 ler

      ierro=0
      det=1.d0
      icont=0
      iconv=20
      jconv=0

c
c     Fixed sky background
c

      b(nterms+1)=a(nterms+1)


c
c     Evaluates alpha and beta matrices
c

 
      do j=1,nterms
      beta(j) = 0.d0
      do k=1,j
      alpha(j,k) = 0.d0
      enddo
      enddo

c
c     2D Circular Gaussian
c

      IF (key.eq.2) THEN

      do 52 i=ny,my

      call circul (raio,xref,yref,ixref,i,ichave)
      if (ichave.lt.0) go to 52


      do 5002 l=nx,mx

      if (imagem(l,i).lt.-9) go to 5002

      call circul (raio,xref,yref,l,i,ichave)
      if (ichave.lt.0) go to 5002

      call dpsf2 (icofsp,l,i,a,deriv,ierro)

      if (ierro.eq.1) return

      icont=icont+1


      do j=1,nterms
      beta(j)=beta(j)+weight(l,i)*(pixmat(l,i)-psf2(icofsp,l,i,a,ierro))
     ?*deriv(j)
      if (ierro.eq.1) return
      do k=1,j
      alpha(j,k)=alpha(j,k)+weight(l,i)*deriv(j)*deriv(k)
      enddo
      enddo

 5002 continue
   52 continue

      ENDIF




c
c     2D Elliptical Gaussian
c

      IF (key.eq.3) THEN

      do 53 i=ny,my

      call circul (raio,xref,yref,ixref,i,ichave)
      if (ichave.lt.0) go to 53

      do 5003 l=nx,mx

      if (imagem(l,i).lt.-9) go to 5003

      call circul (raio,xref,yref,l,i,ichave)
      if (ichave.lt.0) go to 5003

      call dpsf3 (icofsp,l,i,a,deriv,ierro)

      if (ierro.eq.1) return

      icont=icont+1

      do j=1,nterms
      beta(j)=beta(j)+weight(l,i)*(pixmat(l,i)-psf3(icofsp,l,i,a,ierro))
     ?*deriv(j)
      if (ierro.eq.1) return
      do k=1,j
      alpha(j,k)=alpha(j,k)+weight(l,i)*deriv(j)*deriv(k)
      enddo
      enddo

 5003 continue
   53 continue

      ENDIF




c
c     2D Circular Cauchy-Lorentz model
c

      IF (key.eq.4) THEN

      do 54 i=ny,my

      call circul (raio,xref,yref,ixref,i,ichave)
      if (ichave.lt.0) go to 54

      do 5004 l=nx,mx

      if (imagem(l,i).lt.-9) go to 5004

      call circul (raio,xref,yref,l,i,ichave)
      if (ichave.lt.0) go to 5004

      call dpsf4 (icofsp,l,i,a,deriv,ierro)

      if (ierro.eq.1) return

      icont=icont+1

      do j=1,nterms
      beta(j)=beta(j)+weight(l,i)*(pixmat(l,i)-psf4(icofsp,l,i,a,ierro))
     ?*deriv(j)
      if (ierro.eq.1) return
      do k=1,j
      alpha(j,k)=alpha(j,k)+weight(l,i)*deriv(j)*deriv(k)
      enddo
      enddo

 5004 continue
   54 continue

      ENDIF




c
c     2D Elliptical Cauchy-Lorentz model
c

      IF (key.eq.5) THEN

      do 55 i=ny,my

      call circul (raio,xref,yref,ixref,i,ichave)
      if (ichave.lt.0) go to 55

      do 5005 l=nx,mx

      if (imagem(l,i).lt.-9) go to 5005

      call circul (raio,xref,yref,l,i,ichave)
      if (ichave.lt.0) go to 5005

      call dpsf5 (icofsp,l,i,a,deriv,ierro)

      if (ierro.eq.1) return

      icont=icont+1


      do j=1,nterms
      beta(j)=beta(j)+weight(l,i)*(pixmat(l,i)-psf5(icofsp,l,i,a,ierro))
     ?*deriv(j)
      if (ierro.eq.1) return
      do k=1,j
      alpha(j,k)=alpha(j,k)+weight(l,i)*deriv(j)*deriv(k)
      enddo
      enddo

 5005 continue
   55 continue

      ENDIF



c
c     Number of degrees of freedom
c

      free=icont-nterms

c

      if (free.le.0.d0) go to 110

c

      do j=1,nterms
      do k=1,j
      alpha(k,j)=alpha(j,k)
      enddo
      enddo

c
c     Evaluates chi square at starting point
c


      chisq1=qiquad(idimx,idimy,icofsp,pixmat,imagem,raio,xref,yref,
     ?ixref,nx,ny,mx,my,free,a,weight,key,ierro)

c

      if (ierro.eq.1) return



c				 
c     Invert modified curvature matrix to find new parameters
c

 71   continue

      do j=1,nterms
      do k=1,nterms
      aux=alpha(j,j)*alpha(k,k)
      if (aux.le.0.d0) go to 107
      array(j,k)=alpha(j,k)/dsqrt(aux)
      enddo
      array(j,j)=1.d0+flamda
      enddo


      call matinv (nterms,icofsp,array,det,ierro)


      if (ierro.eq.1) return

c


      do j=1,nterms
      c(j)=a(j)

      do k=1,nterms

      aux=alpha(j,j)*alpha(k,k)

      aa=beta(k)*array(j,k)/dsqrt(aux)

      ler=''
      write (ler,*) aa

      do m=1,32

      if (ler(m:m).eq.'N') then

      jconv=jconv+1

      if (jconv.gt.iconv) go to 107

      flamda=10.d0*flamda

      go to 71

      endif

      enddo

      c(j)=c(j)+aa

      enddo
      enddo

      do m=1,nterms
      b(m)=c(m)
      enddo


c
c     If chi-square increased, increase flamda and try again
c



      if (key.eq.3) then

      b(3)=dabs(b(3))
      b(5)=dabs(b(5))

      el=b(4)**2-4.d0*b(3)*b(5)

      if (el.ge.0.d0) then
      jconv=jconv+1
      if (jconv.gt.iconv) go to 107
      flamda=10.d0*flamda
      go to 71
      endif

      endif


c

      if (key.eq.4) then

      b(3)=dabs(b(3))
      b(5)=dabs(b(5))
      b(6)=dabs(b(6))

      endif

c

      if (key.eq.5) then

      b(3)=dabs(b(3))
      b(5)=dabs(b(5))
      b(7)=dabs(b(7))
      b(8)=dabs(b(8))

      el=b(4)**2-4.d0*b(3)*b(5)

      if (el.ge.0.d0) then
      jconv=jconv+1
      if (jconv.gt.iconv) go to 107
      flamda=10.d0*flamda
      go to 71
      endif

      endif

c
 
      chisqr=qiquad(idimx,idimy,icofsp,pixmat,imagem,raio,xref,yref,
     ?ixref,nx,ny,mx,my,free,b,weight,key,ierro)

c

      if (ierro.eq.1) return

c

      jconv=jconv+1

      if (jconv.gt.iconv) go to 107

      if ((chisq1 - chisqr).ge.0.d0) go to 101

      flamda=10.d0*flamda

      go to 71

c
c     Evaluate parameters and uncertainties
c

 101  continue

      do j=1,nterms
      a(j)=b(j)

      if (alpha(j,j).le.0.d0) go to 107

      aux=array(j,j)/alpha(j,j)

      sigmaa(j)=dsqrt(aux)

      enddo

c

      flamda=flamda/10.d0

      go to 110

c

  107 ierro=1

c

  110 continue


      return
      end



c
c
c     Subroutine dpsf2
c
c     Evaluate the derivatives of the 2D Circular Gaussian function
c     given by function psf2.
c
c
c     a(1) - xc
c     a(2) - yc
c     a(3) - Gaussian sigma
c     a(4) - highness
c     a(5) - sky background
c
c
c
c     Last modified: M. Assafin  19/Jan/2019
c


      subroutine dpsf2 (icofsp,j,i,a,deriv,ierro)

      implicit real*8 (a-h,o-z)

      dimension a(icofsp),deriv(icofsp)

c

      funcao=psf2(icofsp,j,i,a,ierro)-a(5)

      if (ierro.eq.1) return

c
c     Analytical expressions for the derivatives
c

 
      zx=(j-a(1))/a(3)
      zy=(i-a(2))/a(3)

      deriv(1)=zx*funcao/a(3)
      deriv(2)=zy*funcao/a(3)
      deriv(3)=(zx**2+zy**2)*funcao/a(3)
      deriv(4)=funcao/a(4)


      return
      end




c
c
c     Subroutine dpsf3
c
c     Evaluate the derivatives of the 2D Elliptical
c     Gaussian function given by function psf3.
c
c
c     a(1) - xc
c     a(2) - yc
c     a(3) - x**2 momentum
c     a(4) - x*y  momentum
c     a(5) - y**2 momentum
c     a(6) - highness
c     a(7) - sky background
c
c
c
c     Last modified: M. Assafin  19/Jan/2019
c


      subroutine dpsf3 (icofsp,j,i,a,deriv,ierro)

      implicit real*8 (a-h,o-z)

      dimension a(icofsp),deriv(icofsp)

c

      funcao=0.5d0*(psf3(icofsp,j,i,a,ierro)-a(7))

      if (ierro.eq.1) return

c
c     Analytical expressions for the derivatives
c

 
      zx=j-a(1)
      zy=i-a(2)

      deriv(1)=(2.d0*a(3)*zx+a(4)*zy)*funcao

      deriv(2)=(a(4)*zx+2.d0*a(5)*zy)*funcao

      deriv(3)=-funcao*zx**2

      deriv(4)=-zx*zy*funcao

      deriv(5)=-funcao*zy**2

      deriv(6)=funcao/(0.5d0*a(6))


      return
      end



c
c
c     Subroutine dpsf4
c
c     Evaluate the derivatives of the 2D Circular Cauchy-Lorentz
c     model with alpha and beta exponents function given by
c     function psf4.
c
c
c     a(1) - xc
c     a(2) - yc
c     a(3) - sigma of Cauchy-Lorentz PSF
c     a(4) - highness
c     a(5) - alpha exponent
c     a(6) - beta  exponent
c     a(7) - sky background
c
c
c
c     Last modified: M. Assafin  19/Jan/2019
c


      subroutine dpsf4 (icofsp,j,i,a,deriv,ierro)

      implicit real*8 (a-h,o-z)

      dimension a(icofsp),deriv(icofsp)

c


      funcao=psf4(icofsp,j,i,a,ierro)-a(7)


      if (ierro.eq.1) return


c
c     Analytical expressions for the derivatives
c


      rx=j-a(1)
      ry=i-a(2)

      r2=rx**2+ry**2


      if (r2.eq.0.d0) then
 
      deriv(1)=0.d0
      deriv(2)=0.d0
      deriv(3)=0.d0
      deriv(4)=1.d0
      deriv(5)=0.d0
      deriv(6)=0.d0
 
      return
 
      endif


c


      r2a2=r2**(a(5)/2.d0)
      r2a21=r2**(a(5)/2.d0-1.d0)

      a35=a(3)**a(5)

      a56=a(5)*a(6)


      pa=r2a2/a35+1.d0

      pa35=pa*a35


      deriv(1)=a56*funcao*r2a21*rx/(pa35)
 
      deriv(2)=a56*funcao*r2a21*ry/(pa35)
 
      deriv(3)=a56*funcao*r2a2/dabs((a(3))*pa35)

      deriv(4)=funcao/a(4)

      deriv(5)=-funcao*a(6)*(dlog(r2)-2.d0*dlog(a(3)))*r2a2/(2.d0*pa*
     ?a(3))

      deriv(6)=-funcao*dlog(pa)



      return
      end




c
c
c     Subroutine dpsf5
c
c     Evaluate the derivatives of the 2D Elliptical
c     Cauchy-Lorentz model with alpha and beta exponents.
c
c
c     a(1) - xc
c     a(2) - yc
c     a(3) - x**2 momentum
c     a(4) - x*y  momentum
c     a(5) - y**2 momentum
c     a(6) - highness
c     a(7) - alpha exponent
c     a(8) - beta  exponent
c     a(9) - sky background
c
c
c
c     Last modified: M. Assafin  19/Jan/2019
c


      subroutine dpsf5 (icofsp,j,i,a,deriv,ierro)

      implicit real*8 (a-h,o-z)

      dimension a(icofsp),deriv(icofsp)

c

      funcao=psf5(icofsp,j,i,a,ierro)-a(9)

      if (ierro.eq.1) return

c
c     Analytical expressions for the derivatives
c

 
      rx=j-a(1)
      ry=i-a(2)

      rx2=(j-a(1))**2
      ry2=(i-a(2))**2

      rxy=(j-a(1))*(i-a(2))

      r2=a(3)*rx2+a(4)*rxy+a(5)*ry2


      if (r2.eq.0.d0) then
 
      deriv(1)=0.d0
      deriv(2)=0.d0
      deriv(3)=0.d0
      deriv(4)=0.d0
      deriv(5)=0.d0
      deriv(6)=1.d0
      deriv(7)=0.d0
      deriv(8)=0.d0
 
      return
 
      endif



 
      a78=-0.5d0*a(7)*a(8)

      t1=r2**(a(7)/2.d0)+1.d0

      t2=r2**(a(7)/2.d0-1.d0)

      t3=t2/t1


      deriv(1)=a78*funcao*t3*(-2.d0*a(3)*rx-a(4)*ry)
 
      deriv(2)=a78*funcao*t3*(-2.d0*a(5)*ry-a(4)*rx)

      deriv(3)=a78*funcao*rx2*t3

      deriv(4)=a78*funcao*rx*ry*t3

      deriv(5)=a78*funcao*ry2*t3

      deriv(6)=funcao/a(6)

      deriv(7)=-0.5d0*a(8)*funcao*(t1-1.d0)*dlog(r2)/t1

      deriv(8)=-funcao*dlog(t1)




      return
      end




c
c     Function psf2
c
c     2D Circular Gaussian PSF function
c
c
c     a(1) - xc
c     a(2) - yc
c     a(3) - Gaussian sigma
c     a(4) - highness
c     a(5) - sky background
c
c
c
c     Last modified:  M. Assafin   19/Jan/2019
c


      double precision function psf2 (icofsp,j,i,a,ierro)

      implicit real*8 (a-h,o-z)

      dimension a(icofsp)

c

      if (a(3).eq.0.d0.or.a(4).eq.0.d0) then
      ierro=1
      return
      endif

c

      psf2=a(5)+a(4)*dexp(-0.5d0*((j-a(1))**2+(i-a(2))**2)/a(3)**2)

c     a4*dexp(-0.5*((x-a1)**2+(y-a2)**2)/a3**2)

      return
      end




c
c     Function psf3
c
c     2D Elliptical Gaussian PSF function
c
c
c     a(1) - xc
c     a(2) - yc
c     a(3) - x**2 momentum
c     a(4) - x*y  momentum
c     a(5) - y**2 momentum
c     a(6) - highness
c     a(7) - sky background
c
c
c
c     Last modified:  M. Assafin   19/Jan/2019
c


      double precision function psf3 (icofsp,j,i,a,ierro)

      implicit real*8 (a-h,o-z)

      dimension a(icofsp)

c

      if (a(3).eq.0.d0.or.a(5).eq.0.d0.or.a(6).eq.0.d0) then
      ierro=1
      return
      endif

      psf3=a(6)*dexp(-0.5d0*(a(3)*(j-a(1))**2+a(4)*(j-a(1))*(i-a(2))+
     ?a(5)*(i-a(2))**2))+a(7)


c     exp(-0.5*(a3*(x-a1)**2+a4*(y-a1)*(x-a2)+a5*(y-a2)**2))

      return
      end




c
c     Function psf4
c
c     2D Circular Cauchy-Lorentz model with alpha and beta exponents
c
c
c     a(1) - xc
c     a(2) - yc
c     a(3) - sigma of Cauchy-Lorentz PSF
c     a(4) - highness
c     a(5) - alpha exponent
c     a(6) - beta  exponent
c     a(7) - sky background
c
c
c
c     For converting the pure Lorentzian sigma (a3) to
c     the statistical Gaussian sigma, we must use:
c
c
c     Ro(Gaussian)= Ro(Lorentz) / sqrt(2*beta*ln(2))
c
c
c
c     Last modified:  M. Assafin   19/Jan/2019
c


      double precision function psf4 (icofsp,j,i,a,ierro)

      implicit real*8 (a-h,o-z)

      dimension a(icofsp)

c

      if (a(3).eq.0.d0.or.a(4).eq.0.d0) then
      ierro=1
      return
      endif

c


      psf4=a(4)/(1.d0+((((j-a(1))**2+(i-a(2))**2)**(0.5d0))/a(3))**a(5)
     ?)**a(6)+a(7)



      return
      end





c
c     Function psf5
c
c     2D Elliptical Cauchy-Lorentz model with alpha and beta exponents
c
c
c     a(1) - xc
c     a(2) - yc
c     a(3) - x**2 momentum
c     a(4) - x*y  momentum
c     a(5) - y**2 momentum
c     a(6) - highness
c     a(7) - alpha exponent
c     a(8) - beta  exponent
c     a(9) - sky background
c
c
c
c     For converting the pure Lorentzian sigma (a3) to
c     the statistical Gaussian sigma, we must use:
c
c     Ro(Gaussian)= Ro(Lorentz) / sqrt(2*beta*ln(2))
c
c
c     The same relation is used to convert between
c     Lorentzian and statistical Gaussian semi-major
c     and semi-minor axes.
c
c
c
c     Last modified:  M. Assafin   19/Jan/2019
c


      double precision function psf5 (icofsp,j,i,a,ierro)

      implicit real*8 (a-h,o-z)

      dimension a(icofsp)

c

      if (a(3).eq.0.d0.or.a(5).eq.0.d0.or.a(6).eq.0.d0) then
      ierro=1
      return
      endif



      psf5=a(6)/(1.d0+(a(3)*(j-a(1))**2+a(4)*(j-a(1))*(i-a(2))+
     ?a(5)*(i-a(2))**2)**(a(7)/2.d0))**a(8)+a(9)




c     a6/(1+(a3*(x-a1)**2+a4*(x-a1)*(y-a2)+a5*(y-a2)**2)**(a7/2))**a8


      return
      end




c
c     Subroutine rab
c
c
c     For a 2D Circular Cauchy-Lorentz model with alpha and
c     beta exponents, estimates the initial r0 and improves
c     the initial alpha value, for a default initial beta
c     value furnished by the user.
c
c     The (xc,yc) and highness are previously estimated and
c     furnished as input.
c
c     In the procedure, rings are used to estimate average pixel
c     counts.
c
c 
c
c     a(1) - xc
c     a(2) - yc
c     a(3) - sigma of Cauchy-Lorentz PSF (r0)
c     a(4) - highness
c     a(5) - alpha exponent
c     a(6) - beta  exponent
c     a(7) - sky background
c
c
c
c
c
c     Last modified:  M. Assafin   20/Sep/2021
c


      subroutine rab (idimx,idimy,idiobs,pixmat,imagem,sv,ior,nx,ny,
     ?raio,bx,by,alturx,fundox,r0,alfa,beta,ierro,qua)

      implicit real*8 (a-h,o-z)


      integer*2 imagem(idimx,idimy)
      real*4 pixmat(idimx,idimy)

      dimension sv(idiobs),ior(idiobs)



c
c     Initial values
c

      ierro=0

      gwi=1.d0

      iraio=raio



c
c     Counts of last ring radius associated to alpha
c     improving computations. The ring is not necessarily
c     the true last radius to avoid problems with
c     log (0) = infinity.
c

      do i=iraio,1,-1

      ray=i

      call skycic (idimx,idimy,idiobs,sv,ior,pixmat,nx,ny,bx,by,ray,gwi,
     ?qua,sper,sper2,nbg)

      if (nbg.lt.2) then
      ierro=1
      return
      endif

      if (sper.gt.fundox) go to 5

      enddo

 5    clast=alturx/(sper-fundox)

      rlast=ray

      thres=fundox+alturx/(2.d0**beta)


c
c     Estimating r0
c


      do i=1,iraio

      ray=i

      call skycic (idimx,idimy,idiobs,sv,ior,pixmat,nx,ny,bx,by,ray,gwi,
     ?qua,sper,sper2,nbg)

      if (nbg.lt.2) then
      ierro=1
      return
      endif


      if (sper.le.thres) go to 15

      enddo


 15   r0=ray

      alfa=dlog(clast**(1.d0/beta)-1.d0)/dlog(rlast/r0)

      return

      end



c
c
c      Function QIQUAD
c
c      Evaluates reduced chi-square of fit to the data
c
c
c      psf2 - 2D Circular Gaussian
c
c      psf3 - 2D Elliptical Gaussian
c
c      psf4 - 2D Circular Lorentz model
c
c      psf5 - 2D Elliptical Lorentz model
c
c
c
c
c      Last modification: M. Assafin 01/Mar/2022
c
c


      double precision function qiquad (idimx,idimy,icofsp,pixmat,
     ?imagem,raio,xref,yref,ixref,nx,ny,mx,my,free,a,weight,key,ierro)

      implicit real*8 (a-h,o-z)

      real*4 pixmat(idimx,idimy),weight(idimx,idimy)
      integer*2 imagem(idimx,idimy)
 

      dimension a(icofsp)

c

      chisq=0.d0
      qiquad=0.d0

c

      if (free.le.0.d0) then
      ierro=1
      return
      endif


c
c     2D Circular Gaussian
c

      IF (key.eq.2) THEN

      do 32 i=ny,my

      call circul (raio,xref,yref,ixref,i,ichave)
      if (ichave.lt.0) go to 32

      do 352 j=nx,mx

      if (imagem(j,i).lt.-9) go to 352

      call circul (raio,xref,yref,j,i,ichave)
      if (ichave.lt.0) go to 352

      chisq=chisq+weight(j,i)*(pixmat(j,i)-psf2(icofsp,j,i,a,ierro))**2

      if (ierro.eq.1) return

  352 continue
   32 continue

      ENDIF

c
c     2D Elliptical Gaussian
c

      IF (key.eq.3) THEN

      do 33 i=ny,my

      call circul (raio,xref,yref,ixref,i,ichave)
      if (ichave.lt.0) go to 33

      do 353 j=nx,mx

      if (imagem(j,i).lt.-9) go to 353

      call circul (raio,xref,yref,j,i,ichave)
      if (ichave.lt.0) go to 353

      chisq=chisq+weight(j,i)*(pixmat(j,i)-psf3(icofsp,j,i,a,ierro))**2

      if (ierro.eq.1) return

  353 continue
   33 continue

      ENDIF


c
c     2D Circular Cauchy-Lorentz model
c


      IF (key.eq.4) THEN

      do 34 i=ny,my

      call circul (raio,xref,yref,ixref,i,ichave)
      if (ichave.lt.0) go to 34

      do 354 j=nx,mx

      if (imagem(j,i).lt.-9) go to 354

      call circul (raio,xref,yref,j,i,ichave)
      if (ichave.lt.0) go to 354

      chisq=chisq+weight(j,i)*(pixmat(j,i)-psf4(icofsp,j,i,a,ierro))**2

      if (ierro.eq.1) return

  354 continue
   34 continue

      ENDIF


c
c     2D Elliptical Cauchy-Lorentz model
c

      IF (key.eq.5) THEN

      do 35 i=ny,my

      call circul (raio,xref,yref,ixref,i,ichave)
      if (ichave.lt.0) go to 35

      do 355 j=nx,mx

      if (imagem(j,i).lt.-9) go to 355

      call circul (raio,xref,yref,j,i,ichave)
      if (ichave.lt.0) go to 355

      chisq=chisq+weight(j,i)*(pixmat(j,i)-psf5(icofsp,j,i,a,ierro))**2

      if (ierro.eq.1) return

  355 continue
   35 continue

      ENDIF


c
c     Divide by the number of degrees of freedom
c


      qiquad=chisq/free

      return
      end


c
c
c     Subroutine CIRCUL
c
c
c     Verifies if a pixel is within a circle of radius "raio" and center (xc,yc).
c
c     Output key is "ichave":  +1 : pixel within  the circle
c                              -1 : pixel outside the circle
c
c
c    Last modified:  M. Assafin  16/Sep/2015 
c
c

 
      subroutine circul (raio,xc,yc,ix,iy,ichave)

      implicit real*8 (a-h,o-z)

c

      raio2=raio*raio
      radius=(ix-xc)**2+(iy-yc)**2
      if (radius.le.raio2) then
      ichave=1
      else
      ichave=-1
      endif

      return
      end






c
c
c     Subroutine circol
c
c
c       Establishes if a pixel is:
c
c     - totaly outside of a circle (ichave=-1)
c
c     - totaly inside of a circle (ichave=+1)
c
c     - partially inside/outside of a circle (ichave=0)
c
c
c
c        r1    r2   r3
c        -------------
c        |           |
c        |           |
c      r4|  (ix,iy)  |r5
c        |           |
c        |           |
c        -------------
c        r6    r7    r8
c
c
c
c
c     Last update: M. Assafin 17/July/2018
c
c
c
c



      subroutine circol (raio,xc,yc,ix,iy,ichave)
      implicit real*8 (a-h,o-z)

c

      raio2=raio*raio

      r1=(ix-0.5d0-xc)**2+(iy+0.5d0-yc)**2
      r2=(ix      -xc)**2+(iy+0.5d0-yc)**2
      r3=(ix+0.5d0-xc)**2+(iy+0.5d0-yc)**2
      r4=(ix-0.5d0-xc)**2+(iy      -yc)**2
      r5=(ix+0.5d0-xc)**2+(iy      -yc)**2
      r6=(ix-0.5d0-xc)**2+(iy-0.5d0-yc)**2
      r7=(ix      -xc)**2+(iy-0.5d0-yc)**2
      r8=(ix+0.5d0-xc)**2+(iy-0.5d0-yc)**2


c
c     Pixel totaly outside of a circle (ichave=-1)
c

      if(r1.gt.raio2.and.r2.gt.raio2.and.r3.gt.raio2.and.r4.gt.raio2.and
     ?.r5.gt.raio2.and.r6.gt.raio2.and.r7.gt.raio2.and.r8.gt.raio2) then
      ichave=-1
      return
      endif


c
c     Pixel totaly inside of a circle (ichave=+1)
c

      if(r1.lt.raio2.and.r2.lt.raio2.and.r3.lt.raio2.and.r4.lt.raio2.and
     ?.r5.lt.raio2.and.r6.lt.raio2.and.r7.lt.raio2.and.r8.lt.raio2) then
      ichave=+1
      return
      endif


c
c     Pixel partially inside/outside of a circle (ichave=0)
c


      ichave=0

      return
      end







C
C     SUBROUTINE MATINV
C
C     PURPOSE
C       INVERT A SYMMETRIC MATRIX AND CALCULATE ITS DETERMINANT
C
C     USAGE
C       CALL MATINV ( NORDER, DET)
C
C     DESCRIPTION OF PARAMETERS
C       ARRAY  - INPUT MATRIX WICH IS REPLACED BY ITS INVERSE
C       NORDER - DEGREE OF MATRIX (ORDER OF DETERMINANT)
C       DET    - DETERMINANT OF INPUT MATRIX
C
C     SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C       NONE
C
C
c
c
c     Last update: M. Assafin 17/July/2018
c

      SUBROUTINE MATINV (NORDER, icofsp, array, DET,ierro)
      IMPLICIT REAL *8 (A-H,O-Z)
      DIMENSION ARRAY (icofsp,icofsp), IK(icofsp), JK(icofsp)
C
   10 DET = 1.D0
   11 DO 100 K=1, NORDER
C
C        FIND LARGEST ELEMENT ARRAY(I,J) IN REST OF MATRIX
C
      AMAX= 0.D0
   21 do    I=K, NORDER
      DO 30 J=K, NORDER
      IF ((DABS(AMAX) - DABS(ARRAY(I,J))).gt.0.d0) go to 30
      AMAX = ARRAY(I,J)
      IK(K) = I
      JK(K) = J
   30 CONTINUE
      enddo
C
C        INTERCHANGE ROWS AND COLUMNS TO PUT AMAX IN ARRAY(K,K)
C

   31 IF (AMAX.ne.0.d0) go to 41
   32 DET = 0.D0
      GO TO 140
   41 I = IK(K)
      IF ((I-K).lt.0) go to 21
      IF ((I-K).eq.0) go to 51
      IF ((I-K).gt.0) go to 43
   43 do J=1, NORDER
      SAVE = ARRAY(K,J)
      ARRAY(K,J) = ARRAY(I,J)
      ARRAY(I,J) = -SAVE
      enddo
   51 J = JK(K)
      IF ((J-K).lt.0) go to 21
      IF ((J-K).eq.0) go to 61
      IF ((J-K).gt.0) go to 53
   53 do I=1, NORDER
      SAVE = ARRAY(I,K)
      ARRAY (I,K) = ARRAY(I,J)
      ARRAY (I,J) = -SAVE
      enddo
C
C        ACCUMULATE ELEMENTS OF INVERSE MATRIX
C
   61 DO 70 I=1, NORDER
      IF ((I-K).eq.0) go to 70
      IF (AMAX.EQ.0.D0) THEN
      IERRO=1
      RETURN
      ENDIF
      ARRAY(I,K) = -ARRAY(I,K) / AMAX
   70 CONTINUE
   71 do    I=1, NORDER
      DO 80 J=1, NORDER
      IF ((I-K).eq.0) go to 80
      IF ((J-K).eq.0) go to 80
      ARRAY(I,J) = ARRAY(I,J) + ARRAY(I,K)*ARRAY(K,J)
   80 CONTINUE
      enddo
   81 DO 90 J=1, NORDER
      IF ((J-K).eq.0) go to 90
      IF (AMAX.EQ.0.D0) THEN
      IERRO=1
      RETURN
      ENDIF
      ARRAY(K,J) = ARRAY(K,J) / AMAX
   90 CONTINUE
      IF (AMAX.EQ.0.D0) THEN
      IERRO=1
      RETURN
      ENDIF
      ARRAY(K,K) = 1.D0 / AMAX
      DET = DET * AMAX
 100  continue

C
C        RESTORE ORDERING OF MATRIX
C
  101 DO 130 L=1, NORDER
      K = NORDER - L + 1
      J = IK(K)
      IF ((J-K).le.0) go to 111
      do I=1, NORDER
      SAVE = ARRAY(I,K)
      ARRAY(I,K) = -ARRAY(I,J)
      ARRAY(I,J) = SAVE
      enddo
  111 I = JK(K)
      IF ((I-K).le.0) go to 130
      do J=1, NORDER
      SAVE = ARRAY(K,J)
      ARRAY(K,J) = -ARRAY(I,J)
      ARRAY(I,J) = SAVE
      enddo
  130 CONTINUE
  140 CONTINUE
      RETURN
      END



C
C
C     Subroutine ordem
C
C
C
C     Purpose
C
C       Orders data vectors in crescent value order.
C
C
C     Use
C
C     SUBROUTINE ORDEM (IDIM,N,IORDEM,NVAL)
C
C
C     Description of parameters
C
C       IDIM   - vector dimension
C       N      - number of points to be ordered
C       IORDEM - increasing order numbering of NVAL
C       NVAL   - INTEGER data array itself, ORDERED
C
C
C     Subroutines and subprograms required
C
C
C
C     Comments
C
C
C     Based on Heapsort algorithm (Bevington, 1969;
C     Press et al., 1992).
C
C
C     Last modification:  M. Assafin  09/Jan/2017
C
C
C
      SUBROUTINE ORDEM (idiobs,N,IORDEM,NVAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION IORDEM(idiobs),NVAL(idiobs)

c
      if (n.lt.2) return
c

      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=NVAL(L)
          IRA=IORDEM(L)
        ELSE
          RRA=NVAL(IR)
          IRA=IORDEM(IR)
          NVAL(IR)=NVAL(1)
          IORDEM(IR)=IORDEM(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            NVAL(1)=RRA
            IORDEM(1)=IRA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(NVAL(J).LT.NVAL(J+1))J=J+1
          ENDIF
          IF(RRA.LT.NVAL(J))THEN
            NVAL(I)=NVAL(J)
            IORDEM(I)=IORDEM(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        NVAL(I)=RRA
        IORDEM(I)=IRA
      GO TO 10
      END








c
c
c     Subroutine dordem
c
c
c
c     Purpose
c
c       Orders data vectors in crescent value order.
c
c
c     Use
c
c     subroutine dordem (idim,n,ior,val)
c
c
c     Description of parameters
c
c       idim   - dimension of vectors
c       n      - number of points to be ordered
c       indx   - increasing order numbering of array "arr"
c       arr    - REAL data array itself, NOT ORDERED
c
c
c     Subroutines and subprograms required
c
c
c
c     Comments
c
c
c     Based on the Quicksort algorithm (Press et al., 1992,
c     see indexx subroutine).
c    
c
c
c     Last modification:  M. Assafin  09/Jan/2017
c
c

      subroutine dordem(idim,n,indx,arr)

      implicit real*8 (a-h,o-z)

      integer idim
      integer n,indx(idim),m,nstack
      real*8 arr(idim)
      parameter (m=7,nstack=50)
      integer i,indxt,ir,itemp,j,jstack,k,l,istack(nstack)
      real*8 a

c
      if (n.lt.2) return
c

      do 11 j=1,n
        indx(j)=j
11    continue
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.m)then
        do 13 j=l+1,ir
          indxt=indx(j)
          a=arr(indxt)
          do 12 i=j-1,1,-1
            if(arr(indx(i)).le.a)goto 2
            indx(i+1)=indx(i)
12        continue
          i=0
2         indx(i+1)=indxt
13      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        itemp=indx(k)
        indx(k)=indx(l+1)
        indx(l+1)=itemp
        if(arr(indx(l+1)).gt.arr(indx(ir)))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l)).gt.arr(indx(ir)))then
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l+1)).gt.arr(indx(l)))then
          itemp=indx(l+1)
          indx(l+1)=indx(l)
          indx(l)=itemp
        endif
        i=l+1
        j=ir
        indxt=indx(l)
        a=arr(indxt)
3       continue
          i=i+1
        if(arr(indx(i)).lt.a)goto 3
4       continue
          j=j-1
        if(arr(indx(j)).gt.a)goto 4
        if(j.lt.i)goto 5
        itemp=indx(i)
        indx(i)=indx(j)
        indx(j)=itemp
        goto 3
5       indx(l)=indx(j)
        indx(j)=indxt
        jstack=jstack+2
c       if(jstack.gt.nstack)pause 'NSTACK too small in ordem'
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1

      return
      end




c
c
c     Subroutine cordem
c
c     New version of dordem with bug corrections for 2.10 (NR).
c
c     But it does not work!
c
c     
c
c
c
c     Purpose
c
c       Orders data vectors in crescent value order using the
c     Quicksort method.
c
c
c     Use
c
c     subroutine cordem (idim,n,ior,val)
c
c
c     Description of parameters
c
c       idim   - dimension of vectors
c       n      - number of points to be ordered
c       indx   - increasing order numbering of array "arr"
c       arr    - data array itself, NOT ORDERED
c
c
c     Subroutines and subprograms required
c
c
c
c     Comments
c
c
c     Last modification:  M. Assafin  01/Mar/2022
c
c

      subroutine cordem(idim,n,indx,arr)

      implicit real*8 (a-h,o-z)

      parameter (m=7,nstack=50)

c     integer idim
c     integer n,indx(idim),m,nstack
c     real*8 arr(idim)
c     parameter (m=7,nstack=50)
c     integer i,indxt,ir,itemp,j,jstack,k,l,istack(nstack)
c     real*8 a

      dimension indx(idim),arr(idim),istack(nstack)

c
      if (n.lt.2) return
c

      do 11 j=1,n
        indx(j)=j
11    continue
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.m)then
        do 13 j=l+1,ir
          indxt=indx(j)
          a=arr(indxt)
c         do 12 i=j-1,1,-1
          do 12 i=j-1,l,-1
            if(arr(indx(i)).le.a)goto 2
            indx(i+1)=indx(i)
12        continue
c         i=0
          i=l-1
2         indx(i+1)=indxt
13      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        itemp=indx(k)
        indx(k)=indx(l+1)
        indx(l+1)=itemp
        if(arr(indx(l+1)).gt.arr(indx(ir)))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        endif
c       if(arr(indx(l)).gt.arr(indx(ir)))then
c         itemp=indx(l)
c         indx(l)=indx(ir)
        if(arr(indx(l+1)).gt.arr(indx(ir)))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        endif
c       if(arr(indx(l+1)).gt.arr(indx(l)))then
c         itemp=indx(l+1)
c         indx(l+1)=indx(l)
c         indx(l)=itemp
c       endif
        if(arr(indx(l)).gt.arr(indx(l+1)))then
          itemp=indx(l)
          indx(l)=indx(l+1)
          indx(l+1)=itemp
        endif
        i=l+1
        j=ir
c       indxt=indx(l)
        indxt=indx(l+1)
        a=arr(indxt)
3       continue
          i=i+1
        if(arr(indx(i)).lt.a)goto 3
4       continue
          j=j-1
        if(arr(indx(j)).gt.a)goto 4
        if(j.lt.i)goto 5
        itemp=indx(i)
        indx(i)=indx(j)
        indx(j)=itemp
        goto 3
c5      indx(l)=indx(j)
5       indx(l+1)=indx(j)
        indx(j)=indxt
        jstack=jstack+2
c       if(jstack.gt.nstack)pause 'NSTACK too small in ordem'
c       if(jstack.gt.nstack) write (*,*) 'NSTACK too small in ordem'
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1

      return
      end





c
c
c
c     Subroutine gaia
c
c
c
c     Picks up Gaia DR3 stars from a (RA,Dec) region around the field
c     center.
c
c     Data is extracted by a query to the Vizier web service.
c
c
c     - rag1,deg1: (RA,Dec), ICRS, epoch J2016.0 (Vizier Distribution)
c     - erag1, edeg1: (RA,Dec) position error in arcsec for JD CCD observation epoch,
c       considering errors from positions, proper motions and parallax
c     - pmag1: RA proper motion (without the cosDec factor) in arcsec/year
c     - pmdg1: Dec proper motion in arcsec/year
c     - epmag1, epmdg1: proper motion errors in arcsec/year
c     - plg1: parallax in arcsec
c     - eplg1: parallax error in arcsec
c     - cg1mgg: Gaia magnitude in the G band
c     - g2mag1,g2mag2: min max mag G cutoff
c     - kpm:  key to cut stars without proper motions
c     - kpl:  key to cut stars without parallax
c     - kdup: key to cut stars with dupliticy
c
c
c
c      Last modification: M. Assafin 01/Sep/2022
c
c

      subroutine gaia (idiobs,idigai,epoj,rac,dec,ramin,ramax,demin,
     ?demax,g2mag1,g2mag2,kpm,kpl,kdup,rag1,deg1,erag1,edeg1,pmag1,
     ?pmdg1,epmag1,epmdg1,plg1,eplg1,rvg1,cg1mgg,nest,mtira,kat,fcatga)


      implicit real *8 (a-h,o-z)


      dimension rag1(idigai),deg1(idigai),erag1(idigai),edeg1(idigai),
     ?pmag1(idigai),pmdg1(idigai),epmag1(idigai),epmdg1(idigai),
     ?plg1(idigai),eplg1(idigai),cg1mgg(idigai),rvg1(idigai),
     ?mtira(idigai)



c


c     character*350 vizier
      character*358 vizier

      character*24 erase

      character*21 cat,fcatga(idiobs)

      character*5 iend

c     character*118 linha
      character*120 linha

      character*1 isig




c
c     Initial data
c
c

      pi=0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c     epgaia=2015.5d0
      epgaia=2016.0d0

c

      do i=1,idiobs
      mtira(i)=0
      enddo


c
c     The FOV was already retrieved before? If yes, skip Vizier
c     internet download and use the FOV already downloaded.
c


      cat=''

      cat=fcatga(kat)

      if (cat(1:1).ne.' ') then

      open(95,file=cat)

      go to 27

      endif



c
c     Vizier root command string
c

      vizier=''


c     vizier='vizquery -c.bm=000.0000x000.0000 -out=RA_ICRS -out=e_RA_IC
c    ?RS -out=DE_ICRS -out=e_DE_ICRS -out=pmRA -out=e_pmRA -out=pmDE -ou
c    ?t=e_pmDE -out=Plx -out=e_Plx -out=RV -out=Gmag -out=Jmag -out=e_Jm
c    ?ag -out=Hmag -out=e_Hmag -out=Kmag -out=e_Kmag -out=Dup -source=I/
c    ?345/gaia2 -out.max=0000000 -mime=text -c=000.00000000000-000.00000
c    ?000000 > PRAIA_gaia2_0000.cat'


c     vizier='vizquery -c.bm=000.0000x000.0000 -out=RA_ICRS -out=e_RA_IC
c    ?RS -out=DE_ICRS -out=e_DE_ICRS -out=pmRA -out=e_pmRA -out=pmDE -ou
c    ?t=e_pmDE -out=Plx -out=e_Plx -out=RVDR2 -out=Gmag -out=Jmag -out=e
c    ?_Jmag -out=Hmag -out=e_Hmag -out=Kmag -out=e_Kmag -out=Dup -source
c    ?=I/350/gaiaedr3 -out.max=0000000 -mime=text -c=000.00000000000-000
c    ?.00000000000 > PRAIA_gaia3_00000.cat'


      vizier='vizquery -c.bm=000.0000x000.0000 -out=RA_ICRS -out=e_RA_IC
     ?RS -out=DE_ICRS -out=e_DE_ICRS -out=pmRA -out=e_pmRA -out=pmDE -ou
     ?t=e_pmDE -out=Plx -out=e_Plx -out=RV    -out=Gmag -out=Jmag -out=e
     ?_Jmag -out=Hmag -out=e_Hmag -out=Kmag -out=e_Kmag -out=Dup -source
     ?=I/355/gaiadr3  -out.max=0000000 -mime=text -c=000.00000000000-000
     ?.00000000000 > PRAIA_gaia3_00000.cat'



c
c     Catalogue file name root string
c

      cat=''
c     cat='PRAIA_gaia2_0000.cat'
      cat='PRAIA_gaia3_00000.cat'


c
c     FOV retangular sizes in minutes
c

      ybox=(demax-demin)*60.d0

      if (ramax.ge.ramin) then

      xbox=(ramax-ramin)*60.d0

      else

      xbox=(ramin+ramax-360.d0)*60.d0

      endif



c
c     Updating vizier command string
c
c     (Maximum number of stars)
c


      write (vizier(282:288),'(i7.7)') idigai



c
c     Updating vizier command string
c
c     (Rectangular box sizes)
c

      write (vizier(16:23),'(f8.4)') xbox

      write (vizier(25:32),'(f8.4)') ybox


      do i=16,23
 
      if (vizier(i:i).eq.' ') vizier(i:i)='0'

      enddo


      do i=25,32
 
      if (vizier(i:i).eq.' ') vizier(i:i)='0'

      enddo



c
c     Updating vizier command string
c
c     (FOV center)
c




      write (vizier(304:318),'(f15.11)') rac

      write (vizier(320:334),'(f15.11)') dabs(dec)



      if (dec.ge.0.d0) then

      write (vizier(319:319),'(a1)') '+'

      else

      write (vizier(319:319),'(a1)') '-'

      endif 



      do i=304,318
 
      if (vizier(i:i).eq.' ') vizier(i:i)='0'

      enddo


      do i=320,334


      if (vizier(i:i).eq.' ') vizier(i:i)='0'

      enddo



c
c     Definite temporary catalogue file name
c


      do i=1,99999

      write (cat(13:17),'(i5.5)') i

      open(95,file=cat,status='new',err=5)

      go to 10

 5    close (95)

      enddo

      write (*,*)
      write (*,*)'Gaia DR3 catalogue extraction. More than 99999 simulta
     ?neous extractions attempted. Exiting.'
      write (*,*)
      write (*,*)
      stop


c
c     Securing the catalogue file name
c

 10   write (95,*) 'a'
      write (95,*) 'a'
      close (95)


c
c     Updating vizier command string
c

      write (vizier(350:354),'(i5.5)') i



c
c     Preparing temporary catalogue file
c     for usage
c


      erase=''

      erase='rm '

      erase(4:24)=cat

      call system (erase)


c
c     Querying vizier
c


 15   call system(vizier)


c
c     Checks if catalogue file filling up has ended
c

      open(95,file=cat)


 20   close (95)

      open(95,file=cat)

 22   iend=''

      read (95,25,end=20) iend
 25   format(a5)

      if (iend.ne.'#END#') go to 22


      rewind (95)


c
c     Loads Gaia DR3
c

 27   continue

      j=0
      i=0
 28   i=i+1
      iend=''
      read (95,25) iend
      if (iend.eq.'-----') then
      j=j+1
      if (j.eq.2) i1=i
      endif
      if (iend.eq.'#END#') go to 30
      go to 28

 30   i2=i-2


c
c     Missing catalogue data content.
c     Call Vizier again.
c

      if (i2.eq.i1+1) then
      close (95)
      go to 15
      endif


      if (j.lt.2) then
      close (95)
      go to 15
      endif


c

      rewind (95)




c
c     Stores queried Gaia DR3 star data
c 


      do i=1,i1
      read (95,*)
      enddo

      nest=0

c

      do 40 i=i1+1,i2


      nest=nest+1


      linha=''

      read (95,'(a120)') linha

c
c     Flags Gaia DR3 duplicated stars
c

      if (linha(120:120).ne.'0' .and. kdup.ne.0) mtira(nest)=
     ?mtira(nest)+300


c
c     Flags Gaia DR3 stars without proper motions
c

      if ((linha(49:57).eq.'         '.or.linha(66:74).eq.'         ').
     ?and.kpm.ne.0) mtira(nest)=mtira(nest)+30


c
c     Flags Gaia DR3 stars without parallax
c

      if (linha(83:92).eq.'          ' .and. kpl.ne.0) mtira(nest)=
     ?mtira(nest)+3



c
c     Retrieves data
c


      read (linha,35) rag,era,deg,ede,pa,epa,pd,epd,pl,epl,rv,cgmgg,id

c35   format(2(f15.11,1x,f7.4,1x),2(f9.3,1x,f6.3,1x),f10.4,1x,f7.4,1x,
c    ?f7.2,1x,f9.6,1x,i1)

 35   format(2(f15.11,1x,f7.4,1x),2(f9.3,1x,f6.3,1x),f9.4,1x,f7.4,1x,
     ?f7.2,1x,f9.6,1x,i1)
     


c     write(*,35) rag,era,deg,ede,pa,epa,pd,epd,pl,epl,rv,cgmgg,id


c
c     Flags Gaia DR3 stars outside magnitude range
c


      if (cgmgg.lt.g2mag1 .or. cgmgg.gt.g2mag2) mtira(nest)=
     ?mtira(nest)+3000



c
c     Marks Gaia DR3 stars with unknown magnitudes (they are not
c     excluded: G=-9)
c


      if (linha(110:118).eq.'         ') cgmgg=-9.d0




c
c     Declination proper motion in arcsec/yr (in JD years)
c
c

      pmdg1(nest)=pd/1000.d0


c
c     ICRS declination from J2016.0 (Vizier Distribution)
c     to the epoch of observation (deg)
c


      deg1(nest)=deg+(pmdg1(nest)/3600.d0)*(epoj-epgaia)


c
c     Right ascension proper motion in arcsec/yr (in JD years)
c     (it is multiplied by the cosDec factor)
c
c

      pmag1(nest)=pa/1000.d0


c
c     ICRS right ascension from J2016.0 (Vizier Distribution)
c     to the epoch of observation (deg)
c


      rag1(nest)=rag+(pmag1(nest)/3600.d0)*(epoj-epgaia)/dabs(dcos(
     ?grarad*dabs(deg)))



c
c     (RA,DEC) errors at JD of observation, taking into account
c     (RA,DEC) errors at J2016.0, proper motion and parallax errors
c

      erag1(nest)=(dsqrt(era**2+(epa*(epoj-epgaia))**2+epl**2))/1000.d0
      edeg1(nest)=(dsqrt(ede**2+(epd*(epoj-epgaia))**2+epl**2))/1000.d0



c
c     Proper motion errors in arcsec/yr
c

      epmag1(nest)=epa/1000.d0
      epmdg1(nest)=epd/1000.d0


c
c     Parallax (and error) in arcsec
c

      plg1(nest) =pl/1000.d0
      eplg1(nest)=epl/1000.d0


c
c     Radial velocity in Km/s
c

      rvg1(nest)=rv



c
c     G magnitude
c

      cg1mgg(nest)=cgmgg



c
c     debug alfa delta
c
c
c     ra=rag1(nest)/15.d0
c
c     de=deg1(nest)
c
c     dmag=mag/100.d0
c     IAH=ra
c     AM=(ra-IAH)*60.D0
c     IAM=AM
c     SA =(AM-IAM)*60.D0
c     IF (de.LT.0.D0) THEN
c     ISIG='-'
c     de=-de
c     ELSE
c     ISIG='+'
c     ENDIF
c     IDG=de
c     DM=(de-IDG)*60.D0
c     IDM=DM
c     DS=(DM-IDM)*60.D0
c
c     write(*,*) IAH,IAM,SA,'  ',ISIG,IDG,IDM,DS
c     write(*,*) IAH,IAM,SA,'  ',ISIG,IDG,IDM,DS,dmgj(nest),dmgh(nest),
c    ?dmgk(nest)


 40   continue


      close (95)


c
c     Was the limit number of catalogue objects reached?
c


      if (nest.eq.idigai) then

      write (*,*)
      write (*,*)
      write (*,*)
      write (*,*) 'Warning: the maximum allowed number of Gaia stars'
      write (*,*) 'was reached. It is possible that Gaia stars in'
      write (*,*) 'the FOV were not loaded. If that was the case,'
      write (*,*) 'please consider setting up a smaller FOV for'
      write (*,*) 'the Gaia Catalogue extraction.'
      write (*,*)
      write (*,*)

      endif


c
c     Setting up the cleaning of temporary catalogue files
c


      fcatga(kat)=cat


      return
      end





c
c
c     Subrotine stmass
c
c
c
c     Picks up 2MASS stars from a (RA,Dec) region around the field
c     center
c     
c     Data is extracted by a query to the Vizier web service.
c
c
c     - ra2ma,de2ma in degrees: (RA,Dec) from 2MASS PSC
c     - extracted magnitudes: J, H and K
c
c
c
c     Last modification: M. Assafin  01/Mar/2022
c
c


      subroutine stmass (idiobs,idi2ma,rac,dec,ramin,ramax,demin,demax,
     ?ra2ma,de2ma,dmgj,dmgh,dmgk,emgj,emgh,emgk,ddj2,nest,kat,fcat2m)


      implicit real*8 (a-h,o-z)


      dimension ra2ma(idi2ma),de2ma(idi2ma),dmgj(idi2ma),dmgh(idi2ma),
     ?dmgk(idi2ma),emgj(idi2ma),emgh(idi2ma),emgk(idi2ma),ddj2(idi2ma)



      character*238 vizier

      character*24 erase

      character*21 cat,fcat2m(idiobs)

      character*5 iend

c     character*1 isig



c
c     The FOV was already retrieved before? If yes, skip Vizier
c     internet download and use the FOV already downloaded.
c


      cat=''

      cat=fcat2m(kat)

      if (cat(1:1).ne.' ') then

      open(95,file=cat)

      go to 27

      endif




c
c     Vizier root command string
c

      vizier=''


      vizier='vizquery -c.bm=000.0000x000.0000 -out=RAJ2000 -out=DEJ2000
     ? -out=Jmag -out=e_Jmag -out=Hmag -out=e_Hmag -out=Kmag -out=e_Kmag
     ? -out=JD -source=II/246/out -out.max=0000000 -mime=text -c=000.000
     ?00000000-000.00000000000 > PRAIA_2mass_00000.cat'

c
c     Catalogue file name root string
c

      cat=''
      cat='PRAIA_2mass_00000.cat'


c
c     FOV retangular sizes in minutes
c

      ybox=(demax-demin)*60.d0

      if (ramax.ge.ramin) then

      xbox=(ramax-ramin)*60.d0

      else

      xbox=(ramin+ramax-360.d0)*60.d0

      endif




c
c     Updating vizier command string
c
c     (Maximum number of stars)
c


      write (vizier(162:168),'(i7.7)') idi2ma




c
c     Updating vizier command string
c
c     (Rectangular box sizes)
c

      write (vizier(16:23),'(f8.4)') xbox

      write (vizier(25:32),'(f8.4)') ybox


      do i=16,23
 
      if (vizier(i:i).eq.' ') vizier(i:i)='0'

      enddo


      do i=25,32
 
      if (vizier(i:i).eq.' ') vizier(i:i)='0'

      enddo



c
c     Updating vizier command string
c
c     (FOV center)
c


      write (vizier(184:198),'(f15.11)') rac

      write (vizier(200:214),'(f15.11)') dabs(dec)

      if (dec.ge.0.d0) then

      write (vizier(199:199),'(a1)') '+'

      else

      write (vizier(199:199),'(a1)') '-'

      endif 



      do i=184,198
 
      if (vizier(i:i).eq.' ') vizier(i:i)='0'

      enddo


      do i=200,214
 
      if (vizier(i:i).eq.' ') vizier(i:i)='0'

      enddo



c
c     Definite temporary catalogue file name
c


      do i=1,99999

      write (cat(13:17),'(i5.5)') i

      open(95,file=cat,status='new',err=5)

      go to 10

 5    close (95)

      enddo

      write (*,*)
      write (*,*) '2MASS catalogue extraction. More than 9999 simultaneo
     ?us extractions attempted. Exiting.'
      write (*,*)
      write (*,*)
      stop


c
c     Securing the catalogue file name
c

 10   write (95,*) 'a'
      write (95,*) 'a'
      close (95)


c
c     Updating vizier command string
c

      write (vizier(230:234),'(i5.5)') i



c
c     Preparing temporary catalogue file
c     for usage
c


      erase=''

      erase='rm '

      erase(4:24)=cat

      call system (erase)


c
c     Querying vizier
c


 15   call system(vizier)


c
c     Checks if catalogue file filling up has ended
c

      open(95,file=cat)


 20   close (95)

      open(95,file=cat)

 22   iend=''

      read (95,25,end=20) iend
 25   format(a5)

      if (iend.ne.'#END#') go to 22


      rewind (95)


c
c     Loads 2MASS 
c

 27   continue

      j=0
      i=0
 28   i=i+1
      iend=''
      read (95,25) iend
      if (iend.eq.'-----') then
      j=j+1
      if (j.eq.2) i1=i
      endif
      if (iend.eq.'#END#') go to 30
      go to 28

 30   i2=i-2



c
c     Missing catalogue data content.
c     Call Vizier again.
c

      if (i2.eq.i1+1) then
      close (95)
      go to 15
      endif


      if (j.lt.2) then
      close (95)
      go to 15
      endif


c

      rewind (95)


      do i=1,i1
      read (95,*)
      enddo

      nest=0

      do i=i1+1,i2

      read (95,35) ra,de,zmgj,eemgj,zmgh,eemgh,zmgk,eemgk,ddj
 35   format(2(f10.6,1x),6(f6.3,1x),f12.4)

      nest=nest+1

      ra2ma(nest)=ra
      de2ma(nest)=de
      dmgj(nest)=zmgj
      dmgh(nest)=zmgh
      dmgk(nest)=zmgk
      emgj(nest)=eemgj
      emgh(nest)=eemgh
      emgk(nest)=eemgk
      ddj2(nest)=ddj



c
c     debug alfa delta
c
c
c     ra=ra/15.d0
c     dmag=mag/100.d0
c     IAH=ra
c     AM=(ra-IAH)*60.D0
c     IAM=AM
c     SA =(AM-IAM)*60.D0
c     IF (de.LT.0.D0) THEN
c     ISIG='-'
c     de=-de
c     ELSE
c     ISIG='+'
c     ENDIF
c     IDG=de
c     DM=(de-IDG)*60.D0
c     IDM=DM
c     DS=(DM-IDM)*60.D0
c
c     write(*,*) IAH,IAM,SA,'  ',ISIG,IDG,IDM,DS,dmgj(nest),dmgh(nest),
c    ?dmgk(nest)


      enddo


      close (95)



c
c     Was the limit number of catalogue objects reached?
c


      if (nest.eq.idi2ma) then

      write (*,*)
      write (*,*)
      write (*,*)
      write (*,*) 'Warning: the maximum allowed number of 2MASS stars'
      write (*,*) 'was reached. It is possible that 2MASS stars in'
      write (*,*) 'the FOV were not loaded. If that was the case,'
      write (*,*) 'please consider setting up a smaller FOV for'
      write (*,*) 'the 2MASS Catalogue extraction.'
      write (*,*)
      write (*,*)

      endif


c
c     Setting up the cleaning of temporary catalogue files
c


      fcat2m(kat)=cat


      return
      end




c
c
c     Subroutine cuser
c
c
c
c
c     Picks up stars from the user catalogue (PRAIA format)
c
c
c     - raucs,deucs: (RA,Dec) in degrees at epoch epoj of the CCD observation
c     - eraucs, edeucs: (RA,Dec) position error in arcsec for JD CCD observation epoch
c     - pmras: RA proper motion (multiplied by the cosD factor) in arcsec/year
c     - pmdes: Dec proper motion in arcsec/year
c     - epmras, epmdes: proper motion errors in arcsec/year
c     - udmg_s ...: J, H and K magnitudes from 2MASS in the user catalogue (code 99 if absent)
c     - udmgs:  star magnitude in the user catalogue
c
c
c
c     Last modification:   M. Assafin 14/Nov/2016
c
c



      subroutine cuser (idiobs,ifaixa,epoj,rac,dec,ramin,ramax,demin,
     ?demax,raucs,deucs,eraucs,edeucs,pmras,pmdes,epmras,epmdes,udmgjs,
     ?udmghs,udmgks,udmgs,nest)



      implicit real*8 (a-h,o-z)


      dimension raucs(idiobs),deucs(idiobs),eraucs(idiobs),
     ?edeucs(idiobs),pmras(idiobs),pmdes(idiobs),epmras(idiobs),
     ?epmdes(idiobs),udmgjs(idiobs),udmghs(idiobs),udmgks(idiobs),
     ?udmgs(idiobs)


      dimension cxmin(2),cxmax(2)



      character*1 isig
      character*50 ifaixa

      hmsgms(i,j,a)=i+j/60.d0+a/3600.d0
  


c
c     Auxiliary data
c



      idin50=50
      iun=95


      sfac=1.d0



c
c     Determines pointers for user catalogue reading, given the
c     furnished (min,max) RA limits (no acceleration indexes)
c




      if (ramax.ge.ramin) then


      inde=1


      cxmin(1)=ramin*sfac
      cxmax(1)=ramax*sfac


      else



      inde=2


      cxmin(1)=ramin*sfac
      cxmax(1)=360.d0*sfac


      cxmin(2)=0.d0
      cxmax(2)=ramax*sfac


      endif



c

      write (*,14) ifaixa
 14   format(1x,'User:  ',a50)



c
c     Reads catalogue in PRAIA's user catalogue format
c


      open (iun,file=ifaixa)


      nest=0



      do 40 i=1,inde

      rewind (iun)

      racmin=cxmin(i)
      racmax=cxmax(i)



 20   continue

      read (iun,21,err=20,end=40) iah,iam,sa,isig,idg,idm,sd,ex,ey,codj,
     ?copma,copmd,erpma,erpmd,codmg,res2mg

 21   format(i2,1x,i2,1x,f7.4,2x,a1,i2,1x,i2,1x,f6.3,2x,2f7.3,2x,f16.8,
     ?1x,4(1x,f7.3),2(1x,f6.3))

c

      ra=15.d0*hmsgms(iah,iam,sa)

      de=hmsgms(idg,idm,sd)
      if (isig.eq.'-') de=-de



c

      if (copma.lt.90.d0 .and. copmd.lt.90.d0) then

      cop=2000d0+(codj-2451545d0)/365.25d0
      dt=epoj-cop

      de=de+(copmd*dt)/3600.d0

      ra=ra+(copma*dt/dcos(grarad*dabs(de)))/3600.d0

c     ra=ra+(copma*dt)/3600.d0

      endif



c
c     Checks if star falls inside (RA,Dec) limits
c


      if (ra.lt.racmin) go to 20
      if (ra.gt.racmax) go to 20

      if (de.lt.demin) go to 20
      if (de.gt.demax) go to 20




c
c     Stores star data
c


      nest=nest+1

      if (nest.gt.idiobs) go to 45


      raucs(nest)=ra
      deucs(nest)=de
      eraucs(nest)=ex
      edeucs(nest)=ey

      pmras(nest)=copma
      pmdes(nest)=copmd
      epmras(nest)=erpma
      epmdes(nest)=erpmd


      udmgjs(nest)=99.9d0
      udmghs(nest)=99.9d0
      udmgks(nest)=99.9d0
      udmgs(nest)=codmg



c
c     debug alfa delta
c
c
c     ra=ra/15.d0
c     dmag=dmg4
c     IAH=ra
c     AM=(ra-IAH)*60.D0
c     IAM=AM
c     SA =(AM-IAM)*60.D0
c     IF (de.LT.0.D0) THEN
c     ISIG='-'
c     de=-de
c     ELSE
c     ISIG='+'
c     ENDIF 
c     IDG=de
c     DM=(de-IDG)*60.D0
c     IDM=DM
c     DS=(DM-IDM)*60.D0
c
c     write(*,*) IAH,IAM,SA,'  ',ISIG,IDG,IDM,DS,'  ',ex,ey,
c    ?pmras(nest),pmdes(nest),erpma,erpmd,udmgjs(nest),
c    ?udmghs(nest),udmgks(nest),udmgs(nest)
c


      go to 20

c

 40   continue

 45   close (iun)



c
c     Was the limit number of catalogue objects reached?
c

      if (nest.gt.idiobs) then

      nest=idiobs

      write (*,*)
      write (*,*)
      write (*,*)
      write (*,*) 'Warning: the maximum allowed number of User'
      write (*,*) 'Catalogue stars was reached. It is possible that'
      write (*,*) 'User Catalogue stars in the FOV were not loaded.'
      write (*,*) 'If that was the case, please consider setting up a'
      write (*,*) 'smaller FOV for the setup of the User Catalogue.'
      write (*,*)
      write (*,*)

      endif

c

      return
      end







c
c   
c     Subrotine target
c
c
c     Extracts target data in PRAIA format.
c
c
c
c     Variables:
c
c
c     ialvos => input target file in PRAIA format
c
c     num    => number of targets
c
c     dj     => input Julian Date of image
c
c     tbox   => input epoch error box for target identification in
c               UTC seconds
c
c     tara,tade => (RA,Dec) in degrees
c
c     tadist    => Target distance (km)
c
c     tamag     => Target magnitude
c
c     tafase    => Target solar phase angle (degrees)
c
c     tarad     => Target radius (km)
c
c     tawl      => Target wavelength (um)
c
c     iobalv    => Target label
c
c
c     Last modification:  M. Assafin   19/Jan/2019
c
c

      subroutine target (idiobs,idin50,ialvos,dj,tbox,tara,tade,tadist,
     ?tamag,tafase,tarad,tawl,iobalv,num)


      IMPLICIT REAL *8 (A-H,O-Z)

      dimension tara(idiobs),tade(idiobs),tadist(idiobs),tamag(idiobs),
     ?tafase(idiobs),tarad(idiobs),tawl(idiobs)

      character*(idin50) ialvos

      character*20 iobalv(idiobs),ioba

      character*1 isigas,isigap


      hmsgms(i,j,a)=i+j/60.d0+a/3600.d0


c
c     Initial data
c

      pi    = 0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi


c
c     Converts time error box for Julian Date (fraction of Julian day)
c

      boxepo=tbox/86400.d0


c
c     Opens target file in PRAIA format
c

      num=0

      open (1,file=ialvos,status='old',err=100)

c

      do 20 i=1,idiobs

      read (1,10,end=100) iahas,iamas,asas,isigas,idgas,idmas,dsas,
     ?datalv,ioba,iahap,iamap,asap,isigap,idgap,idmap,dsap,dist,apmag,
     ?fase,radius,wl

 10   format(1x,2(i2,1x),f9.6,1x,a1,i2,1x,i2,1x,f8.5,1x,f16.8,
     ?1x,a20,1x,2(i2,1x),f9.6,1x,a1,i2,1x,i2,1x,f8.5,1x,e23.16,1x,
     ?f5.2,1x,f8.4,1x,f11.3,1x,f7.4)


c
c     All targets with epoch "negative" are accepted.
c
c     For targets with a true Julian Date, only those that match
c     the Julian Date of the image (within the time error box) are
c     accepted and stored.
c
c

      if (datalv.gt.0.d0) then

      dtemp=dabs(datalv-dj)

      if (dtemp.gt.boxepo) go to 20

      endif

c
c     Stores target data.
c

      num=num+1


c
c     Only astrometric (RA,Dec) are kept. Airless apparent (RA,Dec)
c     are discarded.
c
c

      tara(num)=15.d0*hmsgms(iahas,iamas,asas)
      tade(num)=hmsgms(idgas,idmas,dsas)
      if (isigas.eq.'-') tade(num)=-tade(num)


      iobalv(num)=ioba

      tadist(num)=dist

      tamag(num)=apmag

      tafase(num)=fase

      tarad(num)=radius

      tawl(num)=wl


 20   continue


c
c     Finished
c

 100  close (1)

      return
      end




c
c
c
c     Subrotine idxyad
c
c
c     identifies main reference catalogue objects in the FOV.
c
c
c     - id2ma: pointer of each FOV object with the n-th main reference catalogue
c              object
c
c     - ialtu: instrumental object brightness pointer in crescent brightness order
c
c     - ra2ma, de2ma: (RA,Dec)s of main reference catalogue objects
c
c
c     - xob,yob: measured (x,y)s of FOV objects
c
c
c     - xold,yold: (RA,Dec)s of main reference catalogue objects projected in
c                  the tangent plane (sky plane)
c
c     - xra2ma, yde2ma: provisional (RA,Dec)s of all FOV objects in the reference
c                       frame of the main reference catalogue; used in the
c                       identification of FOV objects with the User Catalogue, for
c                       the (RA,Dec) reductions with the User Catalogue
c
c
c     - wsig: weights (not used here)
c
c     - z: standard coordinates for the 4 cte polynomial model
c
c     - u,v,w: variables for LS fitting with SVD.
c
c
c
c     Last modification:   M. Assafin  01/Mar/2022
c
c
c

      subroutine idxyad (idiobs,idigai,icofsp,xest,yest,xp,yp,carx,cary,
     ?scala,erpix,rac,dec,nbcat,nbmed,nest,ialtu,xob,yob,n2mass,id2ma,
     ?ra2ma,de2ma,dmgj,xold,yold,xra2ma,yde2ma,ireflex,ecala,ierro,
     ?ncomum,wsig,u,v,w,z,indux,nx,ny,xrray,yrray,cvm,xragi,iior,jjor)


      implicit real*8 (a-h,o-z)

      parameter(idxynm=18,idxync=21)


      integer*2 ia1c(idnint(nbcat*(nbcat-1)*(nbcat-2)/6.d0))
      integer*2 ia2c(idnint(nbcat*(nbcat-1)*(nbcat-2)/6.d0))
      integer*2 ia3c(idnint(nbcat*(nbcat-1)*(nbcat-2)/6.d0))

      integer*2 ia1m(nbmed*(nbmed-1)*(nbmed-2))
      integer*2 ia2m(nbmed*(nbmed-1)*(nbmed-2))
      integer*2 ia3m(nbmed*(nbmed-1)*(nbmed-2))


      dimension ialtu(idiobs),xob(idiobs),yob(idiobs),id2ma(idiobs),
     ?ra2ma(idigai),de2ma(idigai),dmgj(idigai),xra2ma(idiobs),
     ?yde2ma(idiobs),xold(idigai),yold(idigai),xragi(idigai),
     ?iior(idigai),jjor(idigai)

      dimension coefx(icofsp),coefy(icofsp),ecoefx(icofsp),
     ?ecoefy(icofsp),xcof(icofsp),ycof(icofsp),coef(icofsp),
     ?xest(idiobs),yest(idiobs),xp(idiobs),yp(idiobs)

      dimension wsig(idiobs),z(idiobs),u(idiobs,icofsp),v(icofsp,
     ?icofsp),w(icofsp),cvm(icofsp,icofsp),xrray(icofsp,icofsp),
     ?yrray(icofsp,icofsp)


      dimension lor(50*idnint(nbmed*(nbmed-1)*(nbmed-2)/6.d0))
      dimension mor(50*idnint(nbmed*(nbmed-1)*(nbmed-2)/6.d0))


c     dimension kor(50*idnint(nbmed*(nbmed-1)*(nbmed-2)/6.d0))
c     dimension angu(50*idnint(nbmed*(nbmed-1)*(nbmed-2)/6.d0))


      dimension ipol(7),npol(7)


      character*1 isig
      character*27 ird

      character*(idxync) aca(idnint(nbcat*(nbcat-1)*(nbcat-2)/6.d0)),
     ?verc
      character*(idxynm) ame(nbmed*(nbmed-1)*(nbmed-2)),verm


      dexy(xx,yy,zz,ww)=dsin(yy)*dsin(ww)+dcos(yy)*dcos(ww)*dcos(xx-zz)
      xpad(xx,yy,zz)=dcos(yy)*dsin(xx-zz)
      ypad(xx,yy,zz,ww)=dsin(yy)*dcos(ww)-dcos(yy)*dsin(ww)*dcos(xx-zz)

      alff (xx,yy,zz,ww)=zz+datan2(xx,dcos(ww)-yy*dsin(ww))
      deltt(xx,yy,zz,ww)=datan2((yy*dcos(ww)+dsin(ww))*dcos(xx-zz),
     ?dcos(ww)-yy*dsin(ww))


      ang(a,b,c)=dacos((b**2+c**2-a**2)/(2.d0*b*c))
      side(x1,y1,x2,y2)=dsqrt((x1-x2)**2+(y1-y2)**2)


c
c     Initializing variables
c

c     do i=1,icofsp
c     do j=1,icofsp
c     cvm(j,i)=0.d0
c     enddo
c     enddo


c
c     Auxiliary data
c

      pi    = 0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c

      ierro=0

      epixra=grarad*erpix/3600.d0

      etri=3.d0*(5.d0)**2


c     nang=50*idnint(nbmed*(nbmed-1)*(nbmed-2)/6.d0)


      expfov=2.d0

      div=2.d0

      ndiv=5

c

      ireflex=1

      nptos=3

      in=1
      npto=6


      index=0
      indax=1
 
      nterms=4
      iterms=3


c
c     Normalization factor for (x,y) to avoid overflow in
c     the fit of polynomial models of higher order on
c     large FOVs
c

      facx=dble(nx)
      facy=dble(ny)

c     facx=1.d0
c     facy=1.d0



c
c     Auxiliary formats
c


 2    format(3i6.6)
 3    format(3i7.7)

      aux=idxync/3.d0
      ico=idnint(aux)
      ic1=1
      ic2=ic1+ico-1
      ic3=ic2+1
      ic4=ic3+ico-1
      ic5=ic4+1
      ic6=ic5+ico-1

      aux=idxynm/3.d0
      jco=idnint(aux)
      jc1=1
      jc2=jc1+jco-1
      jc3=jc2+1
      jc4=jc3+jco-1
      jc5=jc4+1
      jc6=jc5+jco-1

c

      do i=1,idiobs
      wsig(i)=1.d0
      enddo

c     write (*,*)
c     write (*,*) 'nstars = ',n2mass
c     write (*,*)
c     stop
      

c
c     Gets angles from all possible measured candidate star triangles
c


      nmed=nbmed

      if (nmed.gt.nest) nmed=nest


      j=0

      do   jj1=1,nmed
      do 6 jj2=1,nmed
      if (jj1.eq.jj2) go to 6
      do 5 jj3=1,nmed
      if (jj1.eq.jj3) go to 5
      if (jj2.eq.jj3) go to 5

      j=j+1

      j1=ialtu(nest-jj1+1)
      j2=ialtu(nest-jj2+1)
      j3=ialtu(nest-jj3+1)

      s1=side(xob(j2),yob(j2),xob(j3),yob(j3))
      s2=side(xob(j1),yob(j1),xob(j3),yob(j3))
      s3=side(xob(j1),yob(j1),xob(j2),yob(j2))

      ia1m(j)=idnint(radgra*60.d0*ang(s1,s2,s3))
      ia2m(j)=idnint(radgra*60.d0*ang(s2,s1,s3))
      ia3m(j)=idnint(radgra*60.d0*ang(s3,s1,s2))

      write (ame(j),2) j1,j2,j3

 5    continue
 6    continue
      enddo


      jj=j



c
c     Projects the main reference catalogue (RA,Dec)s
c     in the tangent plane, and indexes the stars by
c     brightness (magnitudes)
c

      grac=grarad*rac
      gdec=grarad*dec

      do i=1,n2mass

      bra=grarad*ra2ma(i)
      bde=grarad*de2ma(i)
      d=dexy(bra,bde,grac,gdec)

      xold(i)=xpad(bra,bde,grac)/d
      yold(i)=ypad(bra,bde,grac,gdec)/d

      iior(i)=i
      jjor(i)=i
      xragi(i)=dmgj(i)
      if (dmgj(i).lt.-8) xragi(i)=50.d0

      enddo



c
c     Loop that samples different Catalogue FOV sizes
c

      n=n2mass

      ncomum=0


      DO 13 mmmmmm=1,ndiv


      expfov=expfov/div


c
c     Only pics up brighter catalogue stars inside
c     the current sampled FOV
c


      if (mmmmmm.ne.1) then

      barx=carx*expfov
      bary=cary*expfov

      n=0

      do 7 i=1,n2mass

      if (dabs(xold(i)).gt.barx) go to 7
      if (dabs(yold(i)).gt.bary) go to 7

      n=n+1
      iior(n)=n
      jjor(n)=i
      xragi(n)=dmgj(i)
      if (dmgj(i).lt.-8) xragi(n)=50.d0

 7    continue

      endif


c
c     Setup the brightest main reference catalogue stars
c     and measured stars
c
c     (catalogue stars: nbcat first positions in vector iior)
c
c     (measured candidate stars: nbmed first positions in vector ialtu)
c


      if (n.gt.0) call dordem (idigai,n,iior,xragi)


      ncat=nbcat

      if (ncat.gt.n) ncat=n

      if (ncat.lt.3) go to 13


c     write (*,*)
c     write (*,*)
c     write (*,*) 'nest   = ',nest
c     write (*,*) 'n2mass = ',n2mass
c     write (*,*) 'mmmmmm = ',mmmmmm
c     write (*,*) 'n      = ',n
c     write (*,*) 'ncat   = ',ncat
c     write (*,*) 'nmed   = ',nmed
c     write (*,*) 'barx   = ',barx*radgra*60.d0
c     write (*,*) 'bary   = ',bary*radgra*60.d0
c     write (*,*)
c     write (*,*)
c     write (*,*)




c
c     Gets angles from all possible catalogue star triangles
c


      i=0

      do ii1=1,ncat-2
      do ii2=ii1+1,ncat-1
      do ii3=ii2+1,ncat

      i=i+1

      i1=jjor(iior(ii1))
      i2=jjor(iior(ii2))
      i3=jjor(iior(ii3))


      s1=side(xold(i2),yold(i2),xold(i3),yold(i3))
      s2=side(xold(i1),yold(i1),xold(i3),yold(i3))
      s3=side(xold(i1),yold(i1),xold(i2),yold(i2))

      ia1c(i)=idnint(radgra*60.d0*ang(s1,s2,s3))
      ia2c(i)=idnint(radgra*60.d0*ang(s2,s1,s3))
      ia3c(i)=idnint(radgra*60.d0*ang(s3,s1,s2))

      write (aca(i),3) i1,i2,i3

      enddo
      enddo
      enddo


      ii=i


c
c     Finds possible matches within a tolerance given
c     by variable etri (angles in arcminutes)
c

c     write (*,*)
c     write (*,*) 'mmmmmm = ',mmmmmm
c     write (*,*)
c     write (*,*) 'ii,jj  = ',ii,jj
c     write (*,*)



      n=0


      do i=1,ii

      do j=1,jj

      da=(ia1m(j)-ia1c(i))**2+(ia2m(j)-ia2c(i))**2+(ia3m(j)-ia3c(i))**2

      if (da.lt.etri) then

      n=n+1

      lor(n)=i
      mor(n)=j

c     kor(n)=n
c     angu(n)=dsqrt(da)

      endif

      enddo
      enddo


c     write (*,*)
c     write (*,*) 'pegou  = ',n
c     write (*,*)
c
c     stop




c     xlado=10.d0
c
c     call dordem (nang,n,kor,angu)
c
c     do k=1,n
c
c     i=kor(k)
c
c     verm=ame(mor(i))
c     verc=aca(lor(i))
c
c     read (verm(jc1:jc2),*) j1
c     read (verm(jc3:jc4),*) j2
c     read (verm(jc5:jc6),*) j3
c
c     read (verc(ic1:ic2),*) i1
c     read (verc(ic3:ic4),*) i2
c     read (verc(ic5:ic6),*) i3
c  
c
c
c     write (950+mmmmmm,*) i,angu(i),verm,' ',verc
c     write (950+mmmmmm,*) xob(j1),yob(j1),dmgj(i1)
c     write (950+mmmmmm,*) xob(j2),yob(j2),dmgj(i2)
c     write (950+mmmmmm,*) xob(j3),yob(j3),dmgj(i3)
c     write (940+mmmmmm,55) xob(j1),yob(j1),xlado
c     write (940+mmmmmm,55) xob(j2),yob(j2),xlado
c     write (940+mmmmmm,55) xob(j3),yob(j3),xlado
c55   format('circle(',2(f8.2,','),f8.2,')')
c
c     enddo





c
c     No triangle pairs found?
c


      if (n.eq.0) go to 13



c
c     Loop of triangle pairs
c


      do m=1,n

      verc=aca(lor(m))
      verm=ame(mor(m))

      read (verc(ic1:ic2),*) i1
      read (verc(ic3:ic4),*) i2
      read (verc(ic5:ic6),*) i3


      read (verm(jc1:jc2),*) j1
      read (verm(jc3:jc4),*) j2
      read (verm(jc5:jc6),*) j3


      xp(1)=xold(i1)
      yp(1)=yold(i1)

      xp(2)=xold(i2)
      yp(2)=yold(i2)

      xp(3)=xold(i3)
      yp(3)=yold(i3)


      xest(1)=xob(j1)/facx
      yest(1)=yob(j1)/facy

      xest(2)=xob(j2)/facx
      yest(2)=yob(j2)/facy

      xest(3)=xob(j3)/facx
      yest(3)=yob(j3)/facy


c
c     Computes 4 Constant coefficients for the measured
c     and catalogue star triangle pairs
c
c
c     Reflection in x coordinates are considered.
c


      do k=+1,-1,-2

      xest(1)=k*xest(1)
      xest(2)=k*xest(2)
      xest(3)=k*xest(3)

      xest(4)=xest(1)
      yest(4)=yest(1)
      xest(5)=xest(2)
      yest(5)=yest(2)
      xest(6)=xest(3)
      yest(6)=yest(3)

      z(1)=xp(1)
      z(2)=xp(2)
      z(3)=xp(3)
      z(4)=yp(1)
      z(5)=yp(2)
      z(6)=yp(3)


      call svdfit (xest,yest,z,wsig,npto,coef,nterms,u,v,w,idiobs,
     ?icofsp,chisq,idiobs,icofsp,index)


      coefx(1)=coef(1)
      coefx(2)=coef(3)
      coefx(3)=coef(4)

      coefy(1)=coef(2)
      coefy(2)=-coef(4)
      coefy(3)=coef(3)



c
c     Computes the number of identifications (votes)
c     for these 2 triangle pairs
c


      icont=0

      do 10 j=1,nmed

      xx=k*xob(ialtu(nest-j+1))/facx
      yy=yob(ialtu(nest-j+1))/facy


      x=fpol(icofsp,indax,xx,yy,coefx,iterms,in,npto)
      y=fpol(icofsp,indax,xx,yy,coefy,iterms,in,npto)



      do  9 i=1,ncat
      
      dx=dabs(xold(jjor(iior(i)))-x)
      dy=dabs(yold(jjor(iior(i)))-y)
      

      if (dx.gt.epixra) go to 9
      if (dy.gt.epixra) go to 9

      icont=icont+1

      go to 10


 9    continue
 10   continue


c     write (*,*)
c     write (*,*) 'mmmmmm  = ',mmmmmm
c     write (*,*) 'icont   = ',icont
c     write (*,*) 'ireflex = ',ireflex
c     write (*,*)
c     write (*,*)


      if (icont.gt.ncomum) then

      ncomum=icont

      do l=1,3
      xcof(l)=coefx(l)
      ycof(l)=coefy(l)
      enddo


c     xxx1=facx*xest(1)*dble(k)
c     xxx2=facx*xest(2)*dble(k)
c     xxx3=facx*xest(3)*dble(k)
c     yyy1=facy*yest(1)
c     yyy2=facy*yest(2)
c     yyy3=facy*yest(3)


      ireflex=k

c     kkkkkk=mmmmmm


c     write (*,*)
c     write (*,*) 'mmmmmm  = ',kkkkkk
c     write (*,*) 'ncomum  = ',ncomum
c     write (*,*) 'ireflex = ',ireflex
c     write (*,*)
c     write (*,*)


      endif


      enddo


      enddo


c
c     End of FOV size sampling loop
c


 13   CONTINUE


c     xlado=10.d0
c
c     write (950,55) xxx1,yyy1,xlado
c     write (950,55) xxx2,yyy2,xlado
c     write (950,55) xxx3,yyy3,xlado
c55   format('circle(',2(f8.2,','),f8.2,')')



c
c     No common catalogue/measured objects found?
c


      if (ncomum.lt.3) then

      ierro=1

      return

      endif



c
c     Preliminary identification
c

      write (*,*) 
      write (*,*) '(RA,DE) vs. (x,y) catalogue identification'
      write (*,*) '1rst step: 4 Constant Model'
      write (*,*) 

      write (*,*) 'Catalogue objects extracted in field = ',n2mass
      write (*,*) 'Measured objects with (x,y) in field = ',nest
      write (*,*) 'Common catalogue vs. bright objects  = ',ncomum
      write (*,*) 'system reflection flag               = ',ireflex
      write (*,*)
c     write (*,*) 'kkkkkk = ',kkkkkk
      write (*,*)
      write (*,*)

c

      x1=xcof(1)*radgra*3600.d0
      x2=xcof(2)*radgra*3600.d0/facx
      x3=xcof(3)*radgra*3600.d0/facy

      y1=ycof(1)*radgra*3600.d0
      y2=ycof(2)*radgra*3600.d0/facx
      y3=ycof(3)*radgra*3600.d0/facy

c
      write (*,*) 'Coeficients of solution'
      write (*,*)
      write (*,*) 'X = A + Bx + Cy '
      write (*,*) 'Y = D - Cx + By '
      write (*,*)
      write (*,*) 'X: ',x1,x2,x3
      write (*,*) 'Y: ',y1,y2,y3
      write (*,*)


c
c
c     Refines the search for more common measured/catalogue objects.
c
c
c     First, we identify more common measured/catalogue objects, now
c     searching all measured objects and all catalogue objects, by using
c     the 4 CTE model solution obtained with the brightest objects.
c
c     Then, using all these common identifications, we make a new reduction,
c     this time using the Complete 1rst degree polynomial model.
c
c     If this or the 4 CTE model was the polynomial model chosen by the user,
c     the refinement process stops here.
c
c     If the polynomial model chosen by the user is of higher order, then
c     polynomials of higher and higher order are fitted until the model
c     chosen by the user is finally fitted, in accord to the following
c     sequence:
c
c
c     1 - Complete 1rst degree polynomial (3 coefficients per coordinate)
c         (index model 1)
c
c     2 - Complete 2nd degree polynomial  (6 coefficients per coordinate)
c         (index model 2)
c
c     3 - Complete 2nd degree polynomial plus 3rd degree radial distortion
c         term (7 coefficients per coordinate)
c         (index model 4)
c
c     4 - Complete 2nd degree polynomial plus 3rd and 5th degree radial
c         distortion terms (8 coefficients per coordinate)
c         (index model 6)
c
c     5 - Complete 3rd degree polynomial (10 coefficients per coordinate)
c         (index model 3)
c
c     6 - Complete 3rd degree polynomial plus 5th degree radial
c         distortion term (11 coefficients per coordinate)
c         (index model 7)
c
c     7 - Complete 5th degree polynomial  (21 coefficients per coordinate)
c         (index model 5)
c
c
c     No outliers are eliminated in the fittings.
c
c     After each polynomial fit, we make new searches for identifying more
c     and more new common measured/catalogue objects. These new objects are
c     then added to the next fit, until the procedure stops and we get the
c     final set of common measured/catalogue objects.
c     
c
c


c
c     Sets the polynomial model options
c


      index=indux

      if (index.eq.0) index=1

      ipol(1)=1
      ipol(2)=2
      ipol(3)=4
      ipol(4)=6
      ipol(5)=3
      ipol(6)=7
      ipol(7)=5


      npol(1)=3
      npol(2)=6
      npol(3)=7
      npol(4)=8
      npol(5)=10
      npol(6)=11
      npol(7)=21


      m=0

      indox=1
      indoy=1

      nterms=3

      ipass=0


 25   continue


      xm=0.d0
      ym=0.d0
      xs=0.d0
      ys=0.d0

      ncomum=0

      do i=1,idiobs
      id2ma(i)=0
      enddo


      do 30 j=1,nest
      
      xx=ireflex*xob(j)/facx
      yy=yob(j)/facy


      x=fpol(icofsp,indox,xx,yy,xcof,nterms,in,npto)
      y=fpol(icofsp,indoy,xx,yy,ycof,nterms,in,npto)


      do 29 i=1,n2mass

      dx=xold(i)-x
      dy=yold(i)-y


      if (dabs(dx).gt.epixra) go to 29
      if (dabs(dy).gt.epixra) go to 29



c     x1=dx*radgra*3600.d0
c     y1=dy*radgra*3600.d0

c     write (*,*) 'x y dx dy ',xob(j),yob(j),x1,y1



      ncomum=ncomum+1

      id2ma(j)=i
      xp(ncomum)=xold(i)
      yp(ncomum)=yold(i)

c     xest(ncomum)=ireflex*xob(j)/facx
c     yest(ncomum)=yob(j)/facy

      xest(ncomum)=xx
      yest(ncomum)=yy


      xm=xm+dx
      ym=ym+dy

      xs=xs+dx**2
      ys=ys+dy**2


      go to 30


 29   continue

 30   continue



c     write (*,*) 'ncomum = ',ncomum
c
c     call desvio (ncomum,xm,xs)
c     call desvio (ncomum,ym,ys)
c
c     write (*,*) 'xm,ym = ',xm*radgra*3600.d0,ym*radgra*3600.d0
c     write (*,*) 'xs,ys = ',xs*radgra*3600.d0,ys*radgra*3600.d0





      if (ncomum.lt.3) then
      ierro=1
      return
      endif


      if (ipass.ne.0) go to 35


      m=m+1

      indax=ipol(m)
      nterms=npol(m)


      if (indax.eq.1.or.indax.eq.2.or.indax.eq.3.or.indax.eq.5) then

      indox=indax
      indoy=indax

      endif



      if (indax.eq.4.or.indax.eq.6.or.indax.eq.7) then

      indox=indax*10+1
      indoy=indax*10+2

      endif



      call svdfit (xest,yest,xp,wsig,ncomum,xcof,nterms,u,v,w,idiobs,
     ?icofsp,chisq,idiobs,icofsp,indox)


      call svdvar (v,nterms,icofsp,w,cvm,icofsp,icofsp)

      do i=1,nterms
      do j=1,nterms
      xrray(i,j)=cvm(i,j)
      enddo
      enddo



      call svdfit (xest,yest,yp,wsig,ncomum,ycof,nterms,u,v,w,idiobs,
     ?icofsp,chisq,idiobs,icofsp,indoy)


      call svdvar (v,nterms,icofsp,w,cvm,icofsp,icofsp)

      do i=1,nterms
      do j=1,nterms
      yrray(i,j)=cvm(i,j)
      enddo
      enddo


      if (index.eq.ipol(m)) ipass=1


c     write (*,*)
c     write (*,*)
c     write (*,*) 'index = ',index
c     write (*,*) 'indox = ',indox
c     write (*,*) 'indoy = ',indoy
c     write (*,*) 'm     = ',m
c     write (*,*) 'ipol  = ',ipol(m)
c     write (*,*) 'npol  = ',npol(m)
c
c     do i=1,nterms
c     write (*,*) 'xcof ',i,xcof(i)
c     enddo
c
c     do i=1,nterms
c     write (*,*) 'ycof ',i,ycof(i)
c     enddo




      go to 25




 35   continue


c


      write (*,*)
      write (*,*)
      write (*,*) 
      write (*,*) '(RA,DE) vs. (x,y) catalogue identification'
      write (*,*) '*** 2nd final step ***'
      write (*,*) 

      if (index.eq.1) write (*,*) 'Polynomial model = complete 1rst degr
     ?ee'

      if (index.eq.2) write (*,*) 'Polynomial model = complete 2nd degre
     ?e'

      if (index.eq.3) write (*,*) 'Polynomial model = complete 3rd degre
     ?e'

      if (index.eq.4) write (*,*) 'Polynomial model = complete 2nd degre
     ?e + 3rd degree radial distortion term'

      if (index.eq.5) write (*,*) 'Polynomial model = complete 5th degre
     ?e'

      if (index.eq.6) write (*,*) 'Polynomial model = complete 2nd degre
     ?e + 3rd + 5th degree radial distortion terms'

      if (index.eq.7) write (*,*) 'Polynomial model = complete 3rd degre
     ?e + 5th degree radial distortion term'

      write (*,*)  

      write (*,*) 'Catalogue objects extracted in field     = ',n2mass
      write (*,*) 'Measured objects with (x,y) in field     = ',nest
      write (*,*) 'Common catalogue vs. all objects (final) = ',ncomum
      write (*,*) 'system reflection flag                   = ',ireflex
      write (*,*)
      write (*,*)


c
c     Stores final coefficients for the last fitted polynomial model
c


      do i=1,nterms
      coefx(i)=xcof(i)
      coefy(i)=ycof(i)
      enddo



c
c     Coefficient errors
c


      call desvio (ncomum,xm,xs)
      call desvio (ncomum,ym,ys)


c     write (*,*)
c     write (*,*)
c     write (*,*) 'xm,ym = ',xm*radgra*3600.d0,ym*radgra*3600.d0
c     write (*,*) 'xs,ys = ',xs*radgra*3600.d0,ys*radgra*3600.d0
c     write (*,*)
c     write (*,*)


      do k=1,nterms
      ecoefx(k)=xs*dsqrt(xrray(k,k))
      ecoefy(k)=ys*dsqrt(yrray(k,k))
      enddo



c
c     Calculates the pixel scale in arcseconds per pixel
c     and the FOV sizes in arcminutes
c
c


      x1=coefx(1)*radgra*3600.d0
      x2=coefx(2)*radgra*3600.d0/facx
      x3=coefx(3)*radgra*3600.d0/facy

      y1=coefy(1)*radgra*3600.d0
      y2=coefy(2)*radgra*3600.d0/facx
      y3=coefy(3)*radgra*3600.d0/facy


      scala=dsqrt((x2**2+x3**2+y2**2+y3**2)/2.d0)


c
c     Pixel scale error in arcseconds per pixel
c


      ex2=ecoefx(2)*radgra*3600.d0/facx
      ex3=ecoefx(3)*radgra*3600.d0/facy
 
      ey2=ecoefy(2)*radgra*3600.d0/facx
      ey3=ecoefy(3)*radgra*3600.d0/facy

c     ex2=ecoefx(2)
c     ex3=ecoefx(3)
c
c     ey2=ecoefy(2)
c     ey3=ecoefy(3)



      ecala=0.5d0*dsqrt(((x2*ex2)**2+(x3*ex3)**2+(y2*ey2)**2+
     ?(y3*ey3)**2))/scala


c
c     FOV orientation angles
c

      cs=(x2+y3)/2.d0
      sn=(x3-y2)/2.d0

      tet=radgra*theta(cs,sn)
      if (ireflex.lt.0) tet=tet+180.d0
      if (tet.gt.360.d0) tet=tet-360.d0


c
c     FOV center
c

      xx=nx/2.d0
      yy=ny/2.d0


      call xyrade (icofsp,nx,ny,xx,yy,rac,dec,index,ireflex,coefx,coefy,
     ?iah,iam,sa,isig,idg,idm,ds)


      write (ird,37) iah,iam,sa,isig,idg,idm,ds
 37   format(2(i2.2,1x),f7.4,1x,a1,2(i2.2,1x),f6.3)

      if (ird(07:07).eq.' ') ird(07:07)='0'
      if (ird(08:08).eq.' ') ird(08:08)='0'
      if (ird(22:22).eq.' ') ird(22:22)='0'
      if (ird(23:23).eq.' ') ird(23:23)='0'


c
      write (*,*) 
      write (*,*) 'Solution. Linear coeficients (arcseconds):'
      write (*,*) 
      write (*,*) 'X = A + Bx + Cy '
      write (*,*) 'Y = D + Ex + Fy '
      write (*,*)
      write (*,*) 'X: ',x1,x2,x3
      write (*,*) 'Y: ',y1,y2,y3
      write (*,*)
      write (*,*)
      write (*,38) scala
 38   format(1x,'Pixel scale value (arcsec/pixel)     = ',f11.8)
      write (*,*)
      write (*,39) ecala
 39   format(1x,'Pixel scale error (arcsec/pixel)     = ',f11.8)
      write (*,*)
      write (*,40) nx*scala/60.d0,ny*scala/60.d0
 40   format(1x,'FOV (arcminutes) = ',f5.1,' x ',f5.1)
      write (*,*)
      write (*,41) tet
 41   format(1x,'East anti-clockwise from X axis (dg) = ',f8.4)
      write (*,*)
      if (ireflex.gt.0) then
      write (*,*) 'North anti-clockwise from East'
      else
      write (*,*) 'North clockwise from East'
      endif
      write (*,*)
      write (*,42) ird
 42   format(1x,'FOV center (RA,Dec) hh mm ss  dg m s = ',a27)
      write (*,*)
      write (*,43) idnint(nx/2.d0),idnint(ny/2.d0)
 43   format(1x,'FOV center (x,y) in pixels           = ',2(i5,1x))
      write (*,*)
      write (*,*)
      write (*,*)



c
c     Computes provisional (RA,Dec)s of all FOV objects in the reference
c     frame of the main reference catalogue, for posterior identification
c     with the User catalogue objects for (RA,Dec) reductions with the
c     USer catalogue
c



      do i=1,nest


      xx=ireflex*xob(i)/facx
      yy=yob(i)/facy


      x=fpol(icofsp,indox,xx,yy,coefx,nterms,in,npto)
      y=fpol(icofsp,indoy,xx,yy,coefy,nterms,in,npto)


      xra2ma(i)=alff(x,y,grac,gdec)
      yde2ma(i)=deltt(xra2ma(i),y,grac,gdec)


      xra2ma(i)=xra2ma(i)*radgra
      yde2ma(i)=yde2ma(i)*radgra


c
c     debug alfa e delta
c

c     ra=xra2ma(i)
c     de=yde2ma(i)


c     ra=ra/15.d0
c     IAH=ra
c     AM=(ra-IAH)*60.D0
c     IAM=AM
c     SA =(AM-IAM)*60.D0
c     IF (de.LT.0.D0) THEN
c     ISIG='-'
c     de=-de
c     ELSE
c     ISIG='+'
c     ENDIF
c     IDG=de
c     DM=(de-IDG)*60.D0
c     IDM=DM
c     DS=(DM-IDM)*60.D0 
c     write(*,*) IAH,IAM,SA,'  ',ISIG,IDG,IDM,DS



      enddo


      return

      end






c 
c
c     Subroutine posred
c
c
c     (RA,Dec) reduction of (x,y) or (X,Y) *** measured *** positions,
c     with respect to a reference catalogue.
c
c
c     In this version, (RA,Dec) reference catalogue positions are apparent
c     and affected by color refraction.
c
c
c     In this version, the user can weight the (RA,Dec) LS fit.
c
c     Currently, the weights are set by the (x,y) measurement errors. 
c
c
c     The polynomial model that relates reference catalogue (X,Y) coordinates
c     with measured (x,y) or (X,Y) can be one of the following preset
c     models:
c
c
c     index = 0    ===>  4 constants model (scale and orientation) 
c
c     index = 1    ===>  complete 1rst degree polynomial model
c
c     index = 2    ===>  complete 2nd  degree polynomial model
c
c     index = 3    ===>  complete 3rd  degree polynomial model
c
c     index = 4    ===>  complete 2nd degree model + 3rd degree
c     (41,42)            radial distortion term
c
c     index = 5    ===>  complete 5th degree polynomial model
c
c     index = 6    ===>  complete 2nd degree model + 3rd + 5th
c     (61,62)            degree radial distortion terms
c
c     index = 7    ===>  complete 3rd degree model + 5th degree
c     (71,72)            radial distortion term
c
c
c     Note: Internally, indexes 41, 61 and 71 refer separately to
c           X coordinates, and indexes 42, 62 and 72 to Y coordinates.
c
c
c
c     Variables:
c
c     - iw: key for weight (1 ok, 2 no weighting)
c
c     - wsig: weights
c
c     - expix,eypix: input (x,y) errors in pixel units (astrometry mode 0)
c                    or in arcec (astrometry modes 1 and 2)
c
c     - exsec,eysec: output (x,y) errors in arcsec units (all astrometry modes)
c
c     - scala: pixel scale in arcsec/pixel
c
c     - iastro: astrometry mode (0, 1 or 2)
c
c     - corte: elimination of outliers; if corte is negative, a sigma-clip
c              procedure is performed with a factor = |corte|; if corte is
c              positive, one by one, all outliers are eliminated until no
c              point presents |(O-C)s| higher than |corte|
c
c     - z: standard coordinates for the 4 cte polynomial model
c
c     - u,v,w: variables for LS fitting with SVD.
c
c
c
c
c     Outputs:
c
c
c     - (RA,Dec) in degrees (in/out)
c
c     - (x,y)s in pixel units (astrometry modes 0 and 1) or in arcec (mode 2)
c
c     - errors and sigmas in arcsec
c
c     - coefficients (and errors) in radians per pixel, rad per pixel**2 etc
c
c
c
c
c     Last modified: M. Assafin  01/Mar/2022
c
c
c

      subroutine posred (idiobs,idigai,icofsp,ireflex,rac,dec,id,ncat,
     ?racat,decat,nest,xob,yob,xest,yest,xp,yp,corte,nstart,nfinal,ra,
     ?de,era,ede,alfsig,delsig,alfres,delres,coefx,coefy,ecoefx,ecoefy,
     ?itira,avam,dvam,ierro,index,iw,wsig,u,v,w,z,cvm,ior,expix,eypix,
     ?exsec,eysec,scala,iastro,xrray,yrray,array,nx,ny)


      implicit real*8 (a-h,o-z)

      dimension wsig(idiobs),z(idiobs),u(idiobs,icofsp),v(icofsp,
     ?icofsp),w(icofsp),cvm(icofsp,icofsp),coef(icofsp),ecoef(icofsp),
     ?ior(idiobs)

      dimension expix(idiobs),eypix(idiobs),exsec(idiobs),eysec(idiobs)

      dimension id(idiobs),racat(idigai),decat(idigai),xob(idiobs),
     ?yob(idiobs),xp(idiobs),yp(idiobs),xest(idiobs),yest(idiobs)

      dimension ra(idiobs),de(idiobs),era(idiobs),ede(idiobs),
     ?coefx(icofsp),coefy(icofsp),ecoefx(icofsp),ecoefy(icofsp),
     ?alfres(idiobs),delres(idiobs),itira(idiobs),ktira(idiobs),
     ?sao(icofsp),xsao(icofsp),ysao(icofsp),xrray(icofsp,icofsp),
     ?yrray(icofsp,icofsp),array(icofsp,icofsp)

      dimension rae(idiobs),dee(idiobs)

      hmsgms(i,j,a)=i+j/60.d0+a/3600.d0


      dexy(xx,yy,zz,ww)=dsin(yy)*dsin(ww)+dcos(yy)*dcos(ww)*dcos(xx-zz)
      xpad(xx,yy,zz)=dcos(yy)*dsin(xx-zz)
      ypad(xx,yy,zz,ww)=dsin(yy)*dcos(ww)-dcos(yy)*dsin(ww)*dcos(xx-zz)


      alff (xx,yy,zz,ww)=zz+datan2(xx,dcos(ww)-yy*dsin(ww))
      deltt(xx,yy,zz,ww)=datan2((yy*dcos(ww)+dsin(ww))*dcos(xx-zz),
     ?dcos(ww)-yy*dsin(ww))


c
c     Auxiliary data
c


      pi    = 0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c
c     Initializing variables
c

      do i=1,idiobs

      xest(i)=0.d0
      yest(i)=0.d0
      xp(i)=0.d0
      yp(i)=0.d0
      ra(i)=0.d0
      de(i)=0.d0
      era(i)=0.d0
      ede(i)=0.d0
      alfres(i)=0.d0
      delres(i)=0.d0

      ktira(i)=itira(i)
      itira(i)=0

      enddo


      do i=1,icofsp

      coef(i)=0.d0
      ecoef(i)=0.d0

      coefx(i)=0.d0
      coefy(i)=0.d0

      ecoefx(i)=0.d0
      ecoefy(i)=0.d0

      sao(i)=0.d0
      xsao(i)=0.d0
      ysao(i)=0.d0

      do j=1,icofsp
      xrray(j,i)=0.d0
      yrray(j,i)=0.d0
      array(j,i)=0.d0
      cvm(j,i)=0.d0
      enddo

      enddo


c
c     More initial data
c


      if (index.eq.0) nterms=4

      if (index.eq.1) nterms=3

      if (index.eq.2) nterms=6

      if (index.eq.3) nterms=10

      if (index.eq.4) nterms=7

      if (index.eq.5) nterms=21

      if (index.eq.6) nterms=8

      if (index.eq.6) nterms=11


      if (index.eq.4.or.index.eq.6.or.index.eq.7) then

      indox=index*10+1
      indoy=index*10+2

      endif


c
c     Normalization factor in (x,y) to avoid overflow
c     in the fit of polynomial models of high order
c     and large FOVs
c


      facx=dble(nx)
      facy=dble(ny)


c

      grac=grarad*rac
      gdec=grarad*dec

      corta=grarad*corte/3600.d0

c
c     Sets valid points and excludes outliers
c     in the (RA,Dec) reduction loop
c


      nstart=0

      ntira=0


 5    continue

      n=0

      do 10 i=1,nest

      if (id(i).eq.0) go to 10

      if (ktira(i).ne.0) go to 10

      n=n+1

      ior(n)=i

c
c     Loading measurement data of reference objects
c


      xest(n)=ireflex*xob(i)/facx
      yest(n)=yob(i)/facy


c
c     Loading catalogue data of reference objects.
c
c     Gnomonic projection in the tangent plane
c


      bra=grarad*racat(id(i))
      bde=grarad*decat(id(i))
      d=dexy(bra,bde,grac,gdec)

      xp(n)=xpad(bra,bde,grac)/d
      yp(n)=ypad(bra,bde,grac,gdec)/d

c
c     Setting the weights
c

      if (iw.eq.1) then

      wsig(n)=dsqrt(expix(i)**2+eypix(i)**2)

      if (iastro.eq.0) wsig(n)=wsig(n)*scala

      if (wsig(n).gt.0.070d0) wsig(n)=0.070d0

      if (wsig(n).lt.0.001d0) wsig(n)=0.070d0

      if (wsig(n).lt.0.010d0) wsig(n)=0.010d0

      wsig(n)=grarad*wsig(n)/3600.d0

      else

      wsig(n)=1.d0

      endif


 10   continue



      if (nstart.eq.0) nstart=n


c
c     4 cte model
c


      IF (index.eq.0) THEN

      do i=1,n

      xest(n+i)=xest(i)
      yest(n+i)=yest(i)

      wsig(n+i)=wsig(i)

      ior(n+i)=ior(i)

      z(i)  =xp(i)
      z(n+i)=yp(i)

      enddo

      nptos=n*2

      ENDIF


c
c     (RA,Dec) LS fit with SVD method
c


      IF (index.eq.0) THEN

      call svdfit (xest,yest,z,wsig,nptos,coef,nterms,u,v,w,idiobs,
     ?icofsp,chisq,idiobs,icofsp,index)

      ENDIF


      IF (index.eq.1.or.index.eq.2.or.index.eq.3.or.index.eq.5) THEN


      call svdfit (xest,yest,xp,wsig,n,coefx,nterms,u,v,w,idiobs,
     ?icofsp,chisqx,idiobs,icofsp,index)


      call svdfit (xest,yest,yp,wsig,n,coefy,nterms,u,v,w,idiobs,
     ?icofsp,chisqy,idiobs,icofsp,index)


      ENDIF


      IF (index.eq.4.or.index.eq.6.or.index.eq.7) THEN

      call svdfit (xest,yest,xp,wsig,n,coefx,nterms,u,v,w,idiobs,
     ?icofsp,chisqx,idiobs,icofsp,indox)

      call svdfit (xest,yest,yp,wsig,n,coefy,nterms,u,v,w,idiobs,
     ?icofsp,chisqy,idiobs,icofsp,indoy)


      ENDIF


c
c     Set outliers
c


      zz=-1.d14

      xm=0.d0
      xs=0.d0


c
c     4 Cte model (model 0)
c


      IF (index.eq.0) THEN

      do i=1,nptos

      x=fpol(icofsp,index,xest(i),yest(i),coef,nterms,i,nptos)

      xx=x-z(i)

      xm=xm+xx
      xs=xs+xx**2


      if (dabs(xx).gt.zz) then
      jm=ior(i)
      zz=dabs(xx)
      endif

      enddo

      ENDIF


c
c     Models 1, 2, 3, 5
c


      IF (index.eq.1.or.index.eq.2.or.index.eq.3.or.index.eq.5) THEN

      do i=1,n

      x=fpol(icofsp,index,xest(i),yest(i),coefx,nterms,i,n)
      y=fpol(icofsp,index,xest(i),yest(i),coefy,nterms,i,n)

      xx=x-xp(i)
      yy=y-yp(i)


      xm=xm+xx
      xs=xs+xx**2

      xm=xm+yy
      xs=xs+yy**2


      if (dabs(xx).gt.zz) then
      jm=ior(i)
      zz=dabs(xx)
      endif

      if (dabs(yy).gt.zz) then
      jm=ior(i)
      zz=dabs(yy)
      endif

      enddo

      nptos=n*2

      ENDIF


c
c     Models 4, 6, 7
c

      IF (index.eq.4.or.index.eq.6.or.index.eq.7) THEN

      do i=1,n

      x=fpol(icofsp,indox,xest(i),yest(i),coefx,nterms,i,n)

      y=fpol(icofsp,indoy,xest(i),yest(i),coefy,nterms,i,n)

      xx=x-xp(i)
      yy=y-yp(i)

      xm=xm+xx
      xs=xs+xx**2

      xm=xm+yy
      xs=xs+yy**2


      if (dabs(xx).gt.zz) then
      jm=ior(i)
      zz=dabs(xx)
      endif

      if (dabs(yy).gt.zz) then
      jm=ior(i)
      zz=dabs(yy)
      endif

      enddo

      nptos=n*2

      ENDIF


c
c     Sets the outlier in accord to the
c     elimination procedure:
c
c     - corte: elimination of outliers; if corte is negative, a sigma-clip
c              procedure is performed with a factor = |corte|; if corte is
c              positive, one by one, all outliers are eliminated until no
c              point presents |(O-C)s| higher than |corte|
c


      IF (corte.ge.0.d0) THEN


      if (zz.ge.corta) then


      if (index.eq.0) then

      if (nstart-ntira.eq.2) go to 20

      else

      if (nstart-ntira.eq.nterms) go to 20

      endif


      ktira(jm)=1

      ntira=ntira+1


      go to 5

      endif


      ELSE


      call desvio (nptos,xm,xs)

      xm=zz/xs

      if (xm.ge.dabs(corte)) then


      if (index.eq.0) then

      if (nstart-ntira.eq.2) go to 20

      else

      if (nstart-ntira.eq.nterms) go to 20

      endif


      ktira(jm)=1

      ntira=ntira+1


      go to 5

      endif

      ENDIF


 20   continue

c
c     No more outliers to be excluded.
c
c     The last (RA,Dec) reduction is repeated
c     to store definite coefficients, covariance
c     matrix, errors, (O-C)s, etc
c

      nfinal=nstart-ntira


c
c    Last (RA,Dec) LS fit with SVD method
c

c
c    4 Cte model
c

      IF (index.eq.0) THEN

      call svdfit (xest,yest,z,wsig,nptos,coef,nterms,u,v,w,idiobs,
     ?icofsp,chisq,idiobs,icofsp,index)

      call svdvar (v,nterms,icofsp,w,cvm,icofsp,icofsp)


c     coefx(1)=coef(1)
c     coefx(2)=coef(3)
c     coefx(3)=coef(4)
c
c     coefy(1)=coef(2)
c     coefy(2)=-coef(4)
c     coefy(3)=coef(3)

      do i=1,nterms
      do j=1,nterms
      array(i,j)=cvm(i,j)
      enddo
      enddo

c     xrray(1,1)=cvm(1,1)
c     xrray(1,2)=cvm(1,3)
c     xrray(1,3)=cvm(1,4)
c
c     xrray(2,1)=cvm(3,1)
c     xrray(2,2)=cvm(3,3)
c     xrray(2,3)=cvm(3,4)
c
c     xrray(3,1)=cvm(4,1)
c     xrray(3,2)=cvm(4,3)
c     xrray(3,3)=cvm(4,4)
c
c     yrray(1,1)=+cvm(2,2)
c     yrray(1,2)=+cvm(2,4)
c     yrray(1,3)=-cvm(2,3)
c
c     yrray(2,1)=+cvm(4,2)
c     yrray(2,2)=+cvm(4,4)
c     yrray(2,3)=-cvm(4,3)
c
c     yrray(3,1)=-cvm(3,2)
c     yrray(3,2)=-cvm(3,4)
c     yrray(3,3)=+cvm(3,3)


      chisqx=chisq
      chisqy=chisq


      ENDIF


c
c     Models 1, 2, 3, 5
c

      IF (index.eq.1.or.index.eq.2.or.index.eq.3.or.index.eq.5) THEN


      call svdfit (xest,yest,xp,wsig,n,coefx,nterms,u,v,w,idiobs,
     ?icofsp,chisqx,idiobs,icofsp,index)

      call svdvar (v,nterms,icofsp,w,cvm,icofsp,icofsp)

      do i=1,nterms
      do j=1,nterms
      xrray(i,j)=cvm(i,j)
      enddo
      enddo


      call svdfit (xest,yest,yp,wsig,n,coefy,nterms,u,v,w,idiobs,
     ?icofsp,chisqy,idiobs,icofsp,index)


      call svdvar (v,nterms,icofsp,w,cvm,icofsp,icofsp)

      do i=1,nterms
      do j=1,nterms
      yrray(i,j)=cvm(i,j)
      enddo
      enddo


      ENDIF


c
c     Models 4, 6, 7
c

      IF (index.eq.4.or.index.eq.6.or.index.eq.7) THEN

      call svdfit (xest,yest,xp,wsig,n,coefx,nterms,u,v,w,idiobs,
     ?icofsp,chisqx,idiobs,icofsp,indox)

      call svdvar (v,nterms,icofsp,w,cvm,icofsp,icofsp)

      do i=1,nterms
      do j=1,nterms
      xrray(i,j)=cvm(i,j)
      enddo
      enddo


      call svdfit (xest,yest,yp,wsig,n,coefy,nterms,u,v,w,idiobs,
     ?icofsp,chisqy,idiobs,icofsp,indoy)


      call svdvar (v,nterms,icofsp,w,cvm,icofsp,icofsp)

      do i=1,nterms
      do j=1,nterms
      yrray(i,j)=cvm(i,j)
      enddo
      enddo


      ENDIF


c
c     Computes projected (RA,Dec) (standard coordinates in the
c     tangent plane) for all objects, (RA,Dec) standard errors,
c     and stores coefficients and their errors (all in radians here)
c
c     Also converts (x,y) measuring errors (pixels, astrometry mode 0
c     only) in true (RA,Dec) measuring errors. (Here, only standard
c     coordinates X,Y are computed in preparation for RA,Dec).
c


      n1=1
      n2=2


c
c     Main loop
c

      DO i=1,nest

      x=xob(i)*ireflex/facx
      y=yob(i)/facy


c
c     4 Cte model (model 0)
c


      IF (index.eq.0) THEN


c     indox=1
c     iterms=3
c
c     polx=fpol(icofsp,indox,x,y,coefx,iterms,i,nest)
c
c     poly=fpol(icofsp,indox,x,y,coefy,iterms,i,nest)
c


c
c     Reduced (observed) (RA,Dec) coordinates
c


      ra(i)=fpol(icofsp,index,x,y,coef,nterms,n1,n2)

      de(i)=fpol(icofsp,index,x,y,coef,nterms,n2,n2)


c
c     Conversion of PSF errors from (x,y) to (RA,Dec)
c


      if (iastro.eq.0) then

      xmax=-1.d14
      ymax=-1.d14

      ey=-2.d0*eypix(i)

      do ii=1,3

      ey=ey+eypix(i)

      ex=-2.d0*expix(i)

      do jj=1,3

      ex=ex+expix(i)

      xx=fpol(icofsp,index,x+ex/facx,y+ey/facy,coef,nterms,n1,n2)
      yy=fpol(icofsp,index,x+ex/facx,y+ey/facy,coef,nterms,n2,n2)


      if (dabs(xx-ra(i)).gt.xmax) then

      xmax=dabs(xx-ra(i))

      rae(i)=xx

      endif


      if (dabs(yy-de(i)).gt.ymax) then

      ymax=dabs(yy-de(i))

      dee(i)=yy

      endif


      enddo

      enddo



c     rae(i)=fpol(icofsp,index,x+expix(i)/facx,y+eypix(i)/facy,coef,
c    ?nterms,n1,n2)
c
c     dee(i)=fpol(icofsp,index,x+expix(i)/facx,y+eypix(i)/facy,coef,
c    ?nterms,n2,n2)



      endif


c
c     (RA,Dec) standard errors
c

      call funcs (icofsp,index,x,y,xsao,nterms,n1,n2)

      call funcs (icofsp,index,x,y,ysao,nterms,n2,n2)

      do m=1,nterms
      sao(m)=0.d0
      enddo

      do m=1,nterms
      do k=1,nterms
      sao(m)=sao(m)+array(m,k)*xsao(k)
      enddo
      enddo

      do k=1,nterms
      era(i)=era(i)+xsao(k)*sao(k)
      enddo

      era(i)=dsqrt(era(i))

      do m=1,nterms
      sao(m)=0.d0
      enddo

      do m=1,nterms
      do k=1,nterms
      sao(m)=sao(m)+array(m,k)*ysao(k)
      enddo
      enddo

      do k=1,nterms
      ede(i)=ede(i)+ysao(k)*sao(k)
      enddo

      ede(i)=dsqrt(ede(i))


c
c     Coefficients and errors
c

      coefx(1)=coef(1)
      coefx(2)=coef(3)
      coefx(3)=coef(4)

      coefy(1)=coef(2)
      coefy(2)=-coef(4)
      coefy(3)=coef(3)


      ecoefx(1)=dsqrt(array(1,1))
      ecoefx(2)=dsqrt(array(3,3))
      ecoefx(3)=dsqrt(array(4,4))

      ecoefy(1)=dsqrt(array(2,2))
      ecoefy(2)=dsqrt(array(4,4))
      ecoefy(3)=dsqrt(array(3,3))


      ENDIF


c
c     Models 1, 2, 3, 5
c


      IF (index.eq.1.or.index.eq.2.or.index.eq.3.or.index.eq.5) THEN

      ra(i)=fpol(icofsp,index,x,y,coefx,nterms,i,nest)

      de(i)=fpol(icofsp,index,x,y,coefy,nterms,i,nest)



c
c     Conversion of PSF errors from (x,y) to (RA,Dec)
c


      if (iastro.eq.0) then


      xmax=-1.d14
      ymax=-1.d14

      ey=-2.d0*eypix(i)

      do ii=1,3

      ey=ey+eypix(i)

      ex=-2.d0*expix(i)

      do jj=1,3

      ex=ex+expix(i)

      xx=fpol(icofsp,index,x+ex/facx,y+ey/facy,coefx,nterms,i,nest)
      yy=fpol(icofsp,index,x+ex/facx,y+ey/facy,coefy,nterms,i,nest)

      if (dabs(xx-ra(i)).gt.xmax) then

      xmax=dabs(xx-ra(i))

      rae(i)=xx

      endif


      if (dabs(yy-de(i)).gt.ymax) then

      ymax=dabs(yy-de(i))

      dee(i)=yy

      endif


      enddo

      enddo


c     rae(i)=fpol(icofsp,index,x+expix(i)/facx,y+eypix(i)/facy,coefx,
c    ?nterms,i,nest)
c
c     dee(i)=fpol(icofsp,index,x+expix(i)/facx,y+eypix(i)/facy,coefy,
c    ?nterms,i,nest)


      endif


c
c     (RA,Dec) standard errors
c

      call funcs (icofsp,index,x,y,xsao,nterms,i,nest)

      call funcs (icofsp,index,x,y,ysao,nterms,i,nest)

      do m=1,nterms
      sao(m)=0.d0
      enddo

      do m=1,nterms
      do k=1,nterms
      sao(m)=sao(m)+xrray(m,k)*xsao(k)
      enddo
      enddo

      do k=1,nterms
      era(i)=era(i)+xsao(k)*sao(k)
      enddo

      era(i)=dsqrt(era(i))

      do m=1,nterms
      sao(m)=0.d0
      enddo

      do m=1,nterms
      do k=1,nterms
      sao(m)=sao(m)+yrray(m,k)*ysao(k)
      enddo
      enddo

      do k=1,nterms
      ede(i)=ede(i)+ysao(k)*sao(k)
      enddo

      ede(i)=dsqrt(ede(i))


c
c     Coefficient errors
c

      do k=1,nterms
      ecoefx(k)=dsqrt(xrray(k,k))
      ecoefy(k)=dsqrt(yrray(k,k))
      enddo


      ENDIF


c
c     Models 4, 6, 7
c

      IF (index.eq.4.or.index.eq.6.or.index.eq.7) THEN

      ra(i)=fpol(icofsp,indox,x,y,coefx,nterms,i,nest)

      de(i)=fpol(icofsp,indoy,x,y,coefy,nterms,i,nest)


c
c     Conversion of PSF errors from (x,y) to (RA,Dec)
c


      if (iastro.eq.0) then



      xmax=-1.d14
      ymax=-1.d14

      ey=-2.d0*eypix(i)

      do ii=1,3

      ey=ey+eypix(i)

      ex=-2.d0*expix(i)

      do jj=1,3

      ex=ex+expix(i)

      xx=fpol(icofsp,indox,x+ex/facx,y+ey/facy,coefx,nterms,i,nest)
      yy=fpol(icofsp,indoy,x+ex/facx,y+ey/facy,coefy,nterms,i,nest)


      if (dabs(xx-ra(i)).gt.xmax) then

      xmax=dabs(xx-ra(i))

      rae(i)=xx

      endif


      if (dabs(yy-de(i)).gt.ymax) then

      ymax=dabs(yy-de(i))

      dee(i)=yy

      endif


      enddo

      enddo



c     rae(i)=fpol(icofsp,indox,x+expix(i)/facx,y+eypix(i)/facy,coefx,
c    ?nterms,i,nest)
c
c     dee(i)=fpol(icofsp,indoy,x+expix(i)/facx,y+eypix(i)/facy,coefy,
c    ?nterms,i,nest)


      endif



c
c     (RA,Dec) standard errors
c

      call funcs (icofsp,indox,x,y,xsao,nterms,i,nest)

      call funcs (icofsp,indoy,x,y,ysao,nterms,i,nest)

      do m=1,nterms
      sao(m)=0.d0
      enddo

      do m=1,nterms
      do k=1,nterms
      sao(m)=sao(m)+xrray(m,k)*xsao(k)
      enddo
      enddo

      do k=1,nterms
      era(i)=era(i)+xsao(k)*sao(k)
      enddo

      era(i)=dsqrt(era(i))

      do m=1,nterms
      sao(m)=0.d0
      enddo

      do m=1,nterms
      do k=1,nterms
      sao(m)=sao(m)+yrray(m,k)*ysao(k)
      enddo
      enddo

      do k=1,nterms
      ede(i)=ede(i)+ysao(k)*sao(k)
      enddo

      ede(i)=dsqrt(ede(i))


c
c     Coefficient errors
c

      do k=1,nterms
      ecoefx(k)=dsqrt(xrray(k,k))
      ecoefy(k)=dsqrt(yrray(k,k))
      enddo

      ENDIF


      ENDDO


c
c     Converts projected (RA,Dec) to spherical
c     (RA,Dec) in degrees.
c
c     Computes (O-C)s and sigmas and average 
c     errors in arcsec
c
c     Finishes convertion from (x,y) measuring errors (pixels,
c     astrometry mode 0 only) to true (RA,Dec) measuring errors.
c     (Here, from X,Y standard coordinates, spherical RA,Dec are
c     computed and the RA,Dec true measuring errors are obtained).
c
c     If astrometry mode is 1 or 2, copies input measuring errors
c     (already in arcsec) directly to output errors (in arcsec).
c

      xm=0.d0
      xs=0.d0

      ym=0.d0
      ys=0.d0


      DO i=1,nest

      x=ra(i)
      y=de(i)

      ra(i)=alff(x,y,grac,gdec)
      de(i)=deltt(ra(i),y,grac,gdec)

      ra(i)=ra(i)*radgra
      de(i)=de(i)*radgra

      if (id(i).ne.0) then

      alfres(i)=(ra(i)-racat(id(i)))*dcos(grarad*dabs(decat(id(i))))
      delres(i)=de(i)-decat(id(i))

      alfres(i)=3600.d0*alfres(i)
      delres(i)=3600.d0*delres(i)

      if (ktira(i).eq.0) then

      xm=xm+alfres(i)
      ym=ym+delres(i)

      xs=xs+alfres(i)**2
      ys=ys+delres(i)**2

      endif


      endif


c
c     Debug
c
c     if (id(i).eq.0) go to 140
c     j=j+1
c     write(*,*) 'alfres delres ktira = ',j,alfres(j),delres(j),itira(j) 
c     perc=100.d0*ntira/nstart
c     write (*,141) alfsig,delsig,nstart,nfinal,perc,avam,dvam
c141  format(1x,'alfsig delsig NI NF = ',2(1x,f6.3),2(1x,i4),1x,f6.2,
c    ?'%',2(1x,f6.3))


c
c     (RA,Dec) true measuring errors (in arcsec) from (x,y) errors
c     in pixels (only astrometry mode 0).  
c
c     If astrometry mode is 1 or 2, copies input measuring errors
c     (already in arcsec) directly to output errors (in arcsec).
c


      if (iastro.eq.0) then

      x=rae(i)
      y=dee(i)

      rae(i)=alff(x,y,grac,gdec)
      dee(i)=deltt(rae(i),y,grac,gdec)

      rae(i)=rae(i)*radgra
      dee(i)=dee(i)*radgra

      exsec(i)=3600.d0*dabs((rae(i)-ra(i))*dcos(grarad*de(i)))
      eysec(i)=3600.d0*dabs(dee(i)-de(i))

      endif


      if (iastro.eq.1.or.iastro.eq.2) then

      exsec(i)=expix(i)
      eysec(i)=eypix(i)

      endif


      ENDDO

c
c     Sigmas of (RA,Dec) reduction in arcsec
c

      call desvio (nfinal,xm,xs)

      call desvio (nfinal,ym,ys)

      alfsig=xs

      delsig=ys




c
c     Unbiased coefficient errors
c

      if (iw.ne.1) then

      do k=1,nterms
      ecoefx(k)=grarad*alfsig*ecoefx(k)/3600.d0
      ecoefy(k)=grarad*delsig*ecoefy(k)/3600.d0
      enddo

      endif


c
c     Standard errors in arcsec, and
c     standard error statistics
c

      avam=0.d0
      dvam=0.d0

      xvas=0.d0
      yvas=0.d0

      do i=1,nest

      if (iw.ne.1) then

      era(i)=alfsig*era(i)
      ede(i)=delsig*ede(i)

      else

      era(i)=3600.d0*radgra*era(i)
      ede(i)=3600.d0*radgra*ede(i)

      endif

      avam=avam+era(i)
      dvam=dvam+ede(i)

      xvas=xvas+era(i)**2
      yvas=yvas+ede(i)**2

      enddo


      call desvio (nest,avam,xvas)

      call desvio (nest,dvam,yvas)


c
c     Restores index of used/eliminated/flagged reference
c     catalogue objects with respect to all measured objects
c


      do i=1,nest

      if (id(i).ne.0) itira(i)=ktira(i)

      enddo

c

c     write (*,*) 'chix = ',chisqx
c     write (*,*) 'chiy = ',chisqy


      return
      end





c 
c
c     Subroutine posxy
c
c
c     Inverted (RA,Dec) to (x,y) reduction.
c
c
c     (RA,Dec) reduction to (x,y) or (X,Y) *** measured *** positions,
c     with respect to a reference catalogue.
c
c
c     In this version, (RA,Dec) reference catalogue positions are apparent
c     and affected by color refraction.
c
c
c     In this version, the user can weight the (RA,Dec) LS fit.
c
c     Currently, the weights are set by the (x,y) measurement errors. 
c
c
c     The polynomial model that relates reference catalogue (X,Y) coordinates
c     to measured (x,y) or (X,Y) can be one of the following preset
c     models:
c
c
c     index = 0    ===>  4 constants model (scale and orientation) 
c
c     index = 1    ===>  complete 1rst degree polynomial model
c
c     index = 2    ===>  complete 2nd  degree polynomial model
c
c     index = 3    ===>  complete 3rd  degree polynomial model
c
c     index = 4    ===>  complete 2nd degree model + 3rd degree
c     (41,42)            radial distortion term
c
c     index = 5    ===>  complete 5th degree polynomial model
c
c     index = 6    ===>  complete 2nd degree model + 3rd + 5th
c     (61,62)            degree radial distortion terms
c
c     index = 7    ===>  complete 3rd degree model + 5th degree
c     (71,72)            radial distortion term
c
c
c     Note: Internally, indexes 41, 61 and 71 refer separately to
c           X coordinates, and indexes 42, 62 and 72 to Y coordinates.
c
c
c
c     Variables:
c
c     - iw: key for weight (1 ok, 2 no weighting)
c
c     - wsig: weights
c
c     - expix,eypix: input (x,y) errors in pixel units (astrometry mode 0)
c                    or in arcsec (astrometry modes 1 and 2)
c 
c
c     - scala: pixel scale in arcsec/pixel
c
c     - iastro: astrometry mode (0, 1 or 2)
c
c     - corte: elimination of outliers; if corte is negative, a sigma-clip
c              procedure is performed with a factor = |corte|; if corte is
c              positive, one by one, all outliers are eliminated until no
c              point presents |(O-C)s| higher than |corte|
c
c     - z: standard coordinates for the 4 cte polynomial model
c
c     - u,v,w: variables for LS fitting with SVD.
c
c
c
c
c     Outputs:
c
c
c     - (RA,Dec) in degrees (in/out)
c
c     - (x,y)s in pixel units (astrometry modes 0 and 1) or in arcec (mode 2)
c
c     - errors and sigmas in arcsec (converted from pixels using the furnished
c       pixel scale)
c
c     - coefficients (and errors) in pixels per radians, pixels per rad**2 etc
c
c
c
c
c     Last modified: M. Assafin  01/Mar/2022
c
c
c

      subroutine posxy (idiobs,idigai,icofsp,ireflex,rac,dec,id,ncat,
     ?racat,decat,nest,xob,yob,xest,yest,xp,yp,corte,nstart,nfinal,ra,
     ?de,era,ede,alfsig,delsig,alfres,delres,coefx,coefy,ecoefx,ecoefy,
     ?itira,avam,dvam,ierro,index,iw,wsig,u,v,w,z,cvm,ior,expix,eypix,
     ?scala,iastro,xrray,yrray,array)


      implicit real*8 (a-h,o-z)

      dimension wsig(idiobs),z(idiobs),u(idiobs,icofsp),v(icofsp,
     ?icofsp),w(icofsp),cvm(icofsp,icofsp),coef(icofsp),ecoef(icofsp),
     ?ior(idiobs)

      dimension expix(idiobs),eypix(idiobs)

      dimension id(idiobs),racat(idigai),decat(idigai),xob(idiobs),
     ?yob(idiobs),xp(idiobs),yp(idiobs),xest(idiobs),yest(idiobs)

      dimension ra(idigai),de(idigai),era(idigai),ede(idigai),
     ?coefx(icofsp),coefy(icofsp),ecoefx(icofsp),ecoefy(icofsp),
     ?alfres(idiobs),delres(idiobs),itira(idiobs),ktira(idiobs),
     ?sao(icofsp),xsao(icofsp),ysao(icofsp),xrray(icofsp,icofsp),
     ?yrray(icofsp,icofsp),array(icofsp,icofsp)



      hmsgms(i,j,a)=i+j/60.d0+a/3600.d0


      dexy(xx,yy,zz,ww)=dsin(yy)*dsin(ww)+dcos(yy)*dcos(ww)*dcos(xx-zz)
      xpad(xx,yy,zz)=dcos(yy)*dsin(xx-zz)
      ypad(xx,yy,zz,ww)=dsin(yy)*dcos(ww)-dcos(yy)*dsin(ww)*dcos(xx-zz)


      alff (xx,yy,zz,ww)=zz+datan2(xx,dcos(ww)-yy*dsin(ww))
      deltt(xx,yy,zz,ww)=datan2((yy*dcos(ww)+dsin(ww))*dcos(xx-zz),
     ?dcos(ww)-yy*dsin(ww))


c
c     Auxiliary data
c


      pi    = 0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c
c     Initializing variables
c

      do i=1,idigai
      ra(i)=0.d0
      de(i)=0.d0
      era(i)=0.d0
      ede(i)=0.d0
      enddo


      do i=1,idiobs

      xest(i)=0.d0
      yest(i)=0.d0
      xp(i)=0.d0
      yp(i)=0.d0
      alfres(i)=0.d0
      delres(i)=0.d0
      ktira(i)=itira(i)
      itira(i)=0

      enddo


      do i=1,icofsp

      coef(i)=0.d0
      ecoef(i)=0.d0

      coefx(i)=0.d0
      coefy(i)=0.d0

      ecoefx(i)=0.d0
      ecoefy(i)=0.d0

      sao(i)=0.d0
      xsao(i)=0.d0
      ysao(i)=0.d0

      do j=1,icofsp
      xrray(j,i)=0.d0
      yrray(j,i)=0.d0
      array(j,i)=0.d0
      cvm(j,i)=0.d0
      enddo

      enddo


c
c     More initial data
c

      if (index.eq.0) nterms=4

      if (index.eq.1) nterms=3

      if (index.eq.2) nterms=6

      if (index.eq.3) nterms=10

      if (index.eq.4) nterms=7

      if (index.eq.5) nterms=21

      if (index.eq.6) nterms=8

      if (index.eq.6) nterms=11


      if (index.eq.4.or.index.eq.6.or.index.eq.7) then

      indox=index*10+1
      indoy=index*10+2

      endif

c

      grac=grarad*rac
      gdec=grarad*dec

      corta=corte/scala

c
c     Sets valid points and excludes outliers
c     in the (RA,Dec) reduction loop
c


      nstart=0

      ntira=0


 5    continue

      n=0

      do 10 i=1,nest

      if (id(i).eq.0) go to 10

      if (ktira(i).ne.0) go to 10

      n=n+1

      ior(n)=i

c
c     Loading measurement data of reference objects
c


      xp(n)=ireflex*xob(i)
      yp(n)=yob(i)


c
c     Loading catalogue data of reference objects.
c
c     Gnomonic projection in the tangent plane
c


      bra=grarad*racat(id(i))
      bde=grarad*decat(id(i))
      d=dexy(bra,bde,grac,gdec)

      xest(n)=xpad(bra,bde,grac)/d
      yest(n)=ypad(bra,bde,grac,gdec)/d

c
c     Setting the weights
c

      if (iw.eq.1) then

      wsig(n)=dsqrt(expix(i)**2+eypix(i)**2)

      if (iastro.eq.1.or.iastro.eq.2) wsig(n)=wsig(n)/scala

      if (wsig(n).gt.0.070d0/scala) wsig(n)=0.070d0/scala

      if (wsig(n).lt.0.001d0/scala) wsig(n)=0.070d0/scala

      if (wsig(n).lt.0.010d0/scala) wsig(n)=0.010d0/scala


      else

      wsig(n)=1.d0

      endif


 10   continue



      if (nstart.eq.0) nstart=n


c
c     4 cte model
c


      IF (index.eq.0) THEN

      do i=1,n

      xest(n+i)=xest(i)
      yest(n+i)=yest(i)

      wsig(n+i)=wsig(i)

      ior(n+i)=ior(i)

      z(i)  =xp(i)
      z(n+i)=yp(i)

      enddo

      nptos=n*2

      ENDIF


c
c     (RA,Dec) LS fit with SVD method
c


      IF (index.eq.0) THEN

      call svdfit (xest,yest,z,wsig,nptos,coef,nterms,u,v,w,idiobs,
     ?icofsp,chisq,idiobs,icofsp,index)

      ENDIF


      IF (index.eq.1.or.index.eq.2.or.index.eq.3.or.index.eq.5) THEN


      call svdfit (xest,yest,xp,wsig,n,coefx,nterms,u,v,w,idiobs,
     ?icofsp,chisqx,idiobs,icofsp,index)


      call svdfit (xest,yest,yp,wsig,n,coefy,nterms,u,v,w,idiobs,
     ?icofsp,chisqy,idiobs,icofsp,index)


      ENDIF


      IF (index.eq.4.or.index.eq.6.or.index.eq.7) THEN

      call svdfit (xest,yest,xp,wsig,n,coefx,nterms,u,v,w,idiobs,
     ?icofsp,chisqx,idiobs,icofsp,indox)

      call svdfit (xest,yest,yp,wsig,n,coefy,nterms,u,v,w,idiobs,
     ?icofsp,chisqy,idiobs,icofsp,indoy)


      ENDIF


c
c     Set outliers
c


      zz=-1.d14

      xm=0.d0
      xs=0.d0


c
c     4 Cte model (model 0)
c


      IF (index.eq.0) THEN

      do i=1,nptos

      x=fpol(icofsp,index,xest(i),yest(i),coef,nterms,i,nptos)

      xx=x-z(i)

      xm=xm+xx
      xs=xs+xx**2


      if (dabs(xx).gt.zz) then
      jm=ior(i)
      zz=dabs(xx)
      endif

      enddo

      ENDIF


c
c     Models 1, 2, 3, 5
c


      IF (index.eq.1.or.index.eq.2.or.index.eq.3.or.index.eq.5) THEN

      do i=1,n

      x=fpol(icofsp,index,xest(i),yest(i),coefx,nterms,i,n)
      y=fpol(icofsp,index,xest(i),yest(i),coefy,nterms,i,n)

      xx=x-xp(i)
      yy=y-yp(i)


      xm=xm+xx
      xs=xs+xx**2

      xm=xm+yy
      xs=xs+yy**2


      if (dabs(xx).gt.zz) then
      jm=ior(i)
      zz=dabs(xx)
      endif

      if (dabs(yy).gt.zz) then
      jm=ior(i)
      zz=dabs(yy)
      endif

      enddo

      nptos=n*2

      ENDIF


c
c     Models 4, 6, 7
c

      IF (index.eq.4.or.index.eq.6.or.index.eq.7) THEN

      do i=1,n

      x=fpol(icofsp,indox,xest(i),yest(i),coefx,nterms,i,n)

      y=fpol(icofsp,indoy,xest(i),yest(i),coefy,nterms,i,n)

      xx=x-xp(i)
      yy=y-yp(i)

      xm=xm+xx
      xs=xs+xx**2

      xm=xm+yy
      xs=xs+yy**2


      if (dabs(xx).gt.zz) then
      jm=ior(i)
      zz=dabs(xx)
      endif

      if (dabs(yy).gt.zz) then
      jm=ior(i)
      zz=dabs(yy)
      endif

      enddo

      nptos=n*2

      ENDIF


c
c     Sets the outlier in accord to the
c     elimination procedure:
c
c     - corte: elimination of outliers; if corte is negative, a sigma-clip
c              procedure is performed with a factor = |corte|; if corte is
c              positive, one by one, all outliers are eliminated until no
c              point presents |(O-C)s| higher than |corte|
c


      IF (corte.ge.0.d0) THEN


      if (zz.ge.corta) then


      if (index.eq.0) then

      if (nstart-ntira.eq.2) go to 20

      else

      if (nstart-ntira.eq.nterms) go to 20

      endif


      ktira(jm)=1

      ntira=ntira+1


      go to 5

      endif


      ELSE


      call desvio (nptos,xm,xs)

      xm=zz/xs

      if (xm.ge.dabs(corte)) then


      if (index.eq.0) then

      if (nstart-ntira.eq.2) go to 20

      else

      if (nstart-ntira.eq.nterms) go to 20

      endif


      ktira(jm)=1

      ntira=ntira+1


      go to 5

      endif

      ENDIF


 20   continue

c
c     No more outliers to be excluded.
c
c     The last (RA,Dec) reduction is repeated
c     to store definite coefficients, covariance
c     matrix, errors, (O-C)s, etc
c

      nfinal=nstart-ntira


c
c    Last (RA,Dec) LS fit with SVD method
c

c
c    4 Cte model
c

      IF (index.eq.0) THEN

      call svdfit (xest,yest,z,wsig,nptos,coef,nterms,u,v,w,idiobs,
     ?icofsp,chisq,idiobs,icofsp,index)

      call svdvar (v,nterms,icofsp,w,cvm,icofsp,icofsp)


      coefx(1)=coef(1)
      coefx(2)=coef(3)
      coefx(3)=coef(4)
 
      coefy(1)=coef(2)
      coefy(2)=-coef(4)
      coefy(3)=coef(3)

      do i=1,nterms
      do j=1,nterms
      array(i,j)=cvm(i,j)
      enddo
      enddo

c     xrray(1,1)=cvm(1,1)
c     xrray(1,2)=cvm(1,3)
c     xrray(1,3)=cvm(1,4)
c
c     xrray(2,1)=cvm(3,1)
c     xrray(2,2)=cvm(3,3)
c     xrray(2,3)=cvm(3,4)
c
c     xrray(3,1)=cvm(4,1)
c     xrray(3,2)=cvm(4,3)
c     xrray(3,3)=cvm(4,4)
c
c     yrray(1,1)=+cvm(2,2)
c     yrray(1,2)=+cvm(2,4)
c     yrray(1,3)=-cvm(2,3)
c
c     yrray(2,1)=+cvm(4,2)
c     yrray(2,2)=+cvm(4,4)
c     yrray(2,3)=-cvm(4,3)
c
c     yrray(3,1)=-cvm(3,2)
c     yrray(3,2)=-cvm(3,4)
c     yrray(3,3)=+cvm(3,3)


      chisqx=chisq
      chisqy=chisq


      ENDIF


c
c     Models 1, 2, 3, 5
c

      IF (index.eq.1.or.index.eq.2.or.index.eq.3.or.index.eq.5) THEN


      call svdfit (xest,yest,xp,wsig,n,coefx,nterms,u,v,w,idiobs,
     ?icofsp,chisqx,idiobs,icofsp,index)

      call svdvar (v,nterms,icofsp,w,cvm,icofsp,icofsp)

      do i=1,nterms
      do j=1,nterms
      xrray(i,j)=cvm(i,j)
      enddo
      enddo


      call svdfit (xest,yest,yp,wsig,n,coefy,nterms,u,v,w,idiobs,
     ?icofsp,chisqy,idiobs,icofsp,index)


      call svdvar (v,nterms,icofsp,w,cvm,icofsp,icofsp)

      do i=1,nterms
      do j=1,nterms
      yrray(i,j)=cvm(i,j)
      enddo
      enddo


      ENDIF


c
c     Models 4, 6, 7
c

      IF (index.eq.4.or.index.eq.6.or.index.eq.7) THEN

      call svdfit (xest,yest,xp,wsig,n,coefx,nterms,u,v,w,idiobs,
     ?icofsp,chisqx,idiobs,icofsp,indox)

      call svdvar (v,nterms,icofsp,w,cvm,icofsp,icofsp)

      do i=1,nterms
      do j=1,nterms
      xrray(i,j)=cvm(i,j)
      enddo
      enddo


      call svdfit (xest,yest,yp,wsig,n,coefy,nterms,u,v,w,idiobs,
     ?icofsp,chisqy,idiobs,icofsp,indoy)


      call svdvar (v,nterms,icofsp,w,cvm,icofsp,icofsp)

      do i=1,nterms
      do j=1,nterms
      yrray(i,j)=cvm(i,j)
      enddo
      enddo


      ENDIF


c
c     Computes fitted measured (x,y) (or measured X,Y in astrometry
c     mode 2) for all objects, and standard errors (arcsec), and stores
c     coefficients and their errors (all in pixels and pixels/radians here)
c


      n1=1
      n2=2


c
c     Main loop
c


      DO i=1,ncat


      bra=grarad*racat(i)
      bde=grarad*decat(i)
      d=dexy(bra,bde,grac,gdec)

      x=xpad(bra,bde,grac)/d
      y=ypad(bra,bde,grac,gdec)/d



c
c     4 Cte model (model 0)
c


      IF (index.eq.0) THEN


c     indox=1
c     iterms=3
c
c     polx=fpol(icofsp,indox,x,y,coefx,iterms,i,nest)
c
c     poly=fpol(icofsp,indox,x,y,coefy,iterms,i,nest)
c


c
c     Reduced (observed) (RA,Dec) coordinates
c


      ra(i)=fpol(icofsp,index,x,y,coef,nterms,n1,n2)

      ra(i)=ireflex*ra(i)

      de(i)=fpol(icofsp,index,x,y,coef,nterms,n2,n2)


c
c     (RA,Dec) standard errors
c

      call funcs (icofsp,index,x,y,xsao,nterms,n1,n2)

      call funcs (icofsp,index,x,y,ysao,nterms,n2,n2)

      do m=1,nterms
      sao(m)=0.d0
      enddo

      do m=1,nterms
      do k=1,nterms
      sao(m)=sao(m)+array(m,k)*xsao(k)
      enddo
      enddo

      do k=1,nterms
      era(i)=era(i)+xsao(k)*sao(k)
      enddo

      era(i)=dsqrt(era(i))

      do m=1,nterms
      sao(m)=0.d0
      enddo

      do m=1,nterms
      do k=1,nterms
      sao(m)=sao(m)+array(m,k)*ysao(k)
      enddo
      enddo

      do k=1,nterms
      ede(i)=ede(i)+ysao(k)*sao(k)
      enddo

      ede(i)=dsqrt(ede(i))


c
c     Coefficients and errors
c

      coefx(1)=coef(1)
      coefx(2)=coef(3)
      coefx(3)=coef(4)

      coefy(1)=coef(2)
      coefy(2)=-coef(4)
      coefy(3)=coef(3)


      ecoefx(1)=dsqrt(array(1,1))
      ecoefx(2)=dsqrt(array(3,3))
      ecoefx(3)=dsqrt(array(4,4))

      ecoefy(1)=dsqrt(array(2,2))
      ecoefy(2)=dsqrt(array(4,4))
      ecoefy(3)=dsqrt(array(3,3))


      ENDIF


c
c     Models 1, 2, 3, 5
c


      IF (index.eq.1.or.index.eq.2.or.index.eq.3.or.index.eq.5) THEN

      ra(i)=fpol(icofsp,index,x,y,coefx,nterms,i,nest)

      ra(i)=ireflex*ra(i)

      de(i)=fpol(icofsp,index,x,y,coefy,nterms,i,nest)


c
c     (RA,Dec) standard errors
c

      call funcs (icofsp,index,x,y,xsao,nterms,i,nest)

      call funcs (icofsp,index,x,y,ysao,nterms,i,nest)

      do m=1,nterms
      sao(m)=0.d0
      enddo

      do m=1,nterms
      do k=1,nterms
      sao(m)=sao(m)+xrray(m,k)*xsao(k)
      enddo
      enddo

      do k=1,nterms
      era(i)=era(i)+xsao(k)*sao(k)
      enddo

      era(i)=dsqrt(era(i))

      do m=1,nterms
      sao(m)=0.d0
      enddo

      do m=1,nterms
      do k=1,nterms
      sao(m)=sao(m)+yrray(m,k)*ysao(k)
      enddo
      enddo

      do k=1,nterms
      ede(i)=ede(i)+ysao(k)*sao(k)
      enddo

      ede(i)=dsqrt(ede(i))


c
c     Coefficient errors
c

      do k=1,nterms
      ecoefx(k)=dsqrt(xrray(k,k))
      ecoefy(k)=dsqrt(yrray(k,k))
      enddo


      ENDIF


c
c     Models 4, 6, 7
c

      IF (index.eq.4.or.index.eq.6.or.index.eq.7) THEN

      ra(i)=fpol(icofsp,indox,x,y,coefx,nterms,i,nest)

      ra(i)=ireflex*ra(i)

      de(i)=fpol(icofsp,indoy,x,y,coefy,nterms,i,nest)


c
c     (RA,Dec) standard errors
c

      call funcs (icofsp,indox,x,y,xsao,nterms,i,nest)

      call funcs (icofsp,indoy,x,y,ysao,nterms,i,nest)

      do m=1,nterms
      sao(m)=0.d0
      enddo

      do m=1,nterms
      do k=1,nterms
      sao(m)=sao(m)+xrray(m,k)*xsao(k)
      enddo
      enddo

      do k=1,nterms
      era(i)=era(i)+xsao(k)*sao(k)
      enddo

      era(i)=dsqrt(era(i))

      do m=1,nterms
      sao(m)=0.d0
      enddo

      do m=1,nterms
      do k=1,nterms
      sao(m)=sao(m)+yrray(m,k)*ysao(k)
      enddo
      enddo

      do k=1,nterms
      ede(i)=ede(i)+ysao(k)*sao(k)
      enddo

      ede(i)=dsqrt(ede(i))


c
c     Coefficient errors
c

      do k=1,nterms
      ecoefx(k)=dsqrt(xrray(k,k))
      ecoefy(k)=dsqrt(yrray(k,k))
      enddo

      ENDIF


      ENDDO


c
c     Computes (O-C)s and sigmas and average 
c     errors in arcsec
c
c     (O-C)= "(x,y) fitted minus (x,y) measured"
c

      xm=0.d0
      xs=0.d0

      ym=0.d0
      ys=0.d0


      do i=1,nest

c     x=ireflex*xob(i)
c     y=yob(i)


      if (id(i).ne.0) then


c     alfres(i)=ireflex*ra(id(i))-x
c     delres(i)=de(id(i))-y


      alfres(i)=ra(id(i))-xob(i)
      delres(i)=de(id(i))-yob(i)


      alfres(i)=scala*alfres(i)
      delres(i)=scala*delres(i)

      if (ktira(i).eq.0) then

      xm=xm+alfres(i)
      ym=ym+delres(i)

      xs=xs+alfres(i)**2
      ys=ys+delres(i)**2

      endif


      endif


c
c     Debug
c
c     if (id(i).eq.0) go to 140
c     j=j+1
c     write(*,*) 'alfres delres ktira = ',j,alfres(j),delres(j),itira(j) 
c     perc=100.d0*ntira/nstart
c     write (*,141) alfsig,delsig,nstart,nfinal,perc,avam,dvam
c141  format(1x,'alfsig delsig NI NF = ',2(1x,f6.3),2(1x,i4),1x,f6.2,
c    ?'%',2(1x,f6.3))


      enddo


c
c     Sigmas of (RA,Dec) reduction in arcsec
c

      call desvio (nfinal,xm,xs)

      call desvio (nfinal,ym,ys)

      alfsig=xs

      delsig=ys



c
c     Unbiased coefficient errors
c

      if (iw.ne.1) then

      do k=1,nterms
      ecoefx(k)=alfsig*ecoefx(k)/scala
      ecoefy(k)=delsig*ecoefy(k)/scala
      enddo

      endif





c
c     Standard errors in arcsec, and
c     standard error statistics
c

      avam=0.d0
      dvam=0.d0

      xvas=0.d0
      yvas=0.d0

      do i=1,ncat

      if (iw.ne.1) then

      era(i)=alfsig*era(i)
      ede(i)=delsig*ede(i)

      else

      era(i)=scala*era(i)
      ede(i)=scala*ede(i)

      endif

      avam=avam+era(i)
      dvam=dvam+ede(i)

      xvas=xvas+era(i)**2
      yvas=yvas+ede(i)**2

      enddo


      call desvio (ncat,avam,xvas)

      call desvio (ncat,dvam,yvas)


c
c     Restores index of used/eliminated/flagged reference
c     catalogue objects with respect to all measured objects.
c

      do i=1,nest

      if (id(i).ne.0) itira(i)=ktira(i)

      enddo

c

c     write (*,*) 'chix =   ',chisqx
c     write (*,*) 'chiy =   ',chisqy
c     write (*,*) 'alfsig = ',alfsig
c     write (*,*) 'delsig = ',delsig


      return
      end





c
c
c     Subroutine cposxy
c
c
c     Given (RA,Dec) and coefficients of the transformation:
c
c       (RA,Dec) ==>  (X,Y)  ==>  (x,y)
c
c     defined by one of the valid indexed models, returns
c     the (x,y) of the object.
c
c
c     Variables
c
c
c     ireflex - reflection of x coordinate
c
c     rac,dec - center of FOV (degrees)
c
c     ra,de   - (RA,Dec) of the object
c
c     index   - model that relates (RA,Dec) ==> (X,Y) ==> (x,y)
c
c     coefx,coefy - coefficients of the models
c
c     xx,yy   - output (x,y) of object in pixels
c
c
c
c     Last modification:  M. Assafin   01/Mar/2022
c
c

      subroutine cposxy (icofsp,ireflex,rac,dec,ra,de,coefx,coefy,index,
     ?xx,yy)

      implicit real*8 (a-h,o-z)

      dimension coefx(icofsp),coefy(icofsp)


      dexy(xx,yy,zz,ww)=dsin(yy)*dsin(ww)+dcos(yy)*dcos(ww)*dcos(xx-zz)
      xpad(xx,yy,zz)=dcos(yy)*dsin(xx-zz)
      ypad(xx,yy,zz,ww)=dsin(yy)*dcos(ww)-dcos(yy)*dsin(ww)*dcos(xx-zz)



c
c     Auxiliary data
c


      pi    = 0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi


c
c     More initial data
c

      if (index.eq.0) nterms=3

      if (index.eq.1) nterms=3

      if (index.eq.2) nterms=6

      if (index.eq.3) nterms=10

      if (index.eq.4) nterms=7

      if (index.eq.5) nterms=21

      if (index.eq.6) nterms=8

      if (index.eq.6) nterms=11


      if (index.eq.4.or.index.eq.6.or.index.eq.7) then

      indox=index*10+1
      indoy=index*10+2

      endif

c

      i=1
      n=1


      grac=grarad*rac
      gdec=grarad*dec


c
c     Gnomonic projection in the tangent plane
c


      bra=grarad*ra
      bde=grarad*de
      d=dexy(bra,bde,grac,gdec)

      x=xpad(bra,bde,grac)/d
      y=ypad(bra,bde,grac,gdec)/d



c
c     4 Cte model (model 0)
c
c     In the fit made previously with subroutine posxy,
c     for convenience, the true 4 coefficients are
c     converted to (x,y) coefficients, to be used in
c     the same manner as the 1rst degree model (model
c     index 1).
c

      IF (index.eq.0) THEN

      indox=1

      xx=fpol(icofsp,indox,x,y,coefx,nterms,i,n)

      yy=fpol(icofsp,indox,x,y,coefy,nterms,i,n)


      ENDIF


c
c     Models 1, 2, 3, 5
c

      IF (index.eq.1.or.index.eq.2.or.index.eq.3.or.index.eq.5) THEN

      xx=fpol(icofsp,index,x,y,coefx,nterms,i,n)

      yy=fpol(icofsp,index,x,y,coefy,nterms,i,n)


      ENDIF



c
c     Models 4, 6, 7
c

      IF (index.eq.4.or.index.eq.6.or.index.eq.7) THEN

      xx=fpol(icofsp,indox,x,y,coefx,nterms,i,n)

      yy=fpol(icofsp,indoy,x,y,coefy,nterms,i,n)


      ENDIF


      xx=ireflex*xx


      return
      end




c 
c
c     Subroutine xyrade
c
c
c     Computes the (RA,Dec) from (x,y) and gives it in the
c     usual hexagesimal format (hh,mm,ss.ssss,dg,mm,ss.sss)
c
c
c
c
c     Last modified: M. Assafin  01/Mar/2022
c
c

      subroutine xyrade (icofsp,nx,ny,xx,yy,rac,dec,index,ireflex,coefx,
     ?coefy,iah,iam,sa,isig,idg,idm,ds)

      implicit real*8 (a-h,o-z)

      dimension coefx(icofsp),coefy(icofsp)

      character*1 isig

      alff (xx,yy,zz,ww)=zz+datan2(xx,dcos(ww)-yy*dsin(ww))
      deltt(xx,yy,zz,ww)=datan2((yy*dcos(ww)+dsin(ww))*dcos(xx-zz),
     ?dcos(ww)-yy*dsin(ww))


c
c     Initial data
c

      pi    = 0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c
c     FOV center
c

      grac=grarad*rac
      gdec=grarad*dec


c
c     More initial data
c


      if (index.eq.0) nterms=3

      if (index.eq.1) nterms=3

      if (index.eq.2) nterms=6

      if (index.eq.3) nterms=10

      if (index.eq.4) nterms=7

      if (index.eq.5) nterms=21

      if (index.eq.6) nterms=8

      if (index.eq.6) nterms=11


      if (index.eq.4.or.index.eq.6.or.index.eq.7) then

      indox=index*10+1
      indoy=index*10+2

      endif


      i=1
      nest=1


c
c     Normalization factor in (x,y) to avoid overflow
c     in the fit of polynomial models of high order
c     and large FOVs
c


      facx=dble(nx)
      facy=dble(ny)


      x=xx*ireflex/facx
      y=yy/facy



c
c     4 Cte model (model 0)
c

      IF (index.eq.0) THEN

      ra=fpol(icofsp,index,x,y,coefx,nterms,i,nest)

      de=fpol(icofsp,index,x,y,coefy,nterms,i,nest)

      ENDIF


c
c     Models 1, 2, 3, 5
c


      IF (index.eq.1.or.index.eq.2.or.index.eq.3.or.index.eq.5) THEN

      ra=fpol(icofsp,index,x,y,coefx,nterms,i,nest)

      de=fpol(icofsp,index,x,y,coefy,nterms,i,nest)


      ENDIF



c
c     Models 4, 6, 7
c

      IF (index.eq.4.or.index.eq.6.or.index.eq.7) THEN

      ra=fpol(icofsp,indox,x,y,coefx,nterms,i,nest)

      de=fpol(icofsp,indoy,x,y,coefy,nterms,i,nest)


      ENDIF


      x=ra
      y=de

      ra=alff(x,y,grac,gdec)
      de=deltt(ra,y,grac,gdec)

      ra=ra*radgra
      de=de*radgra


      ra=ra/15.d0
      iah=ra
      am=(ra-iah)*60.d0
      iam=am
      sa=(am-iam)*60.d0
      if (de.lt.0.d0) then
      isig='-'
      de=-de
      else
      isig='+'
      endif
      idg=de
      dm=(de-idg)*60.d0
      idm=dm
      ds=(dm-idm)*60.d0 

      return
      end



c
c
c
c     Subroutine results
c
c
c     Writes astrometric results of individual objects for each field
c     for each catalogue (the so called xy PRAIA files)
c
c
c     Variables:
c
c
c     idcat1 - index for (RA,Dec) reduction information for reference catalogue stars
c
c     idcat2 - index for a given catalogue's original magnitudes and proper motion
c              information (values, errors)
c
c     idcat3 - index for 2MASS J, H and K magnitudes and errors for all identified
c              2MASS stars among all measured objects
c
c     ixy    - stores xy file data
c
c
c     nest   - total number of objects in the FOV
c
c     mest   - original total number of objects in the FOV detected by the PRAIA's
c              BOIA method, prior to attempting post-(RA,Dec)-reduction forced
c              detection of objects
c
c     kind   - type of object:
c
c                  BOIA1 - primary detected BOIA object with final (x,y)
c                          measurements from primary detection
c                               
c                  BOIA2 - primary detected BOIA object with final (x,y)
c                          measurements from post-detection
c                               
c                  BOIA3 - post-detected BOIA object with (x,y)
c                          measurements from post-detection
c                               
c                               
c
c
c
c      Last modification: M. Assafin 01/Mar/2022
c
c
c
c


      subroutine results (d99,idiobs,idiob2,idiob3,kxy,nest,mest,ixy,
     ?idcat1,idcat2,idcat3,xob,yob,seng,altu,fgcc,fumag,fumag2,cmag,
     ?omag,cmgj,cmgh,cmgk,oemag,ecmgj,ecmgh,ecmgk,cpma,cpmd,cepma,cepmd,
     ?expix,eypix,era,ede,alfsig,delsig,nstart,nfinal,alfres,decres,
     ?itira,ra,de,iuth,iutm,sut,iutano,iutmes,iutdia,dj,iexps,ichfil,
     ?infits,mchobj,nx,ny,xfdp,yfdp,obtype,kind,scala)


      implicit real *8 (a-h,o-z)


      dimension idcat1(idiobs),idcat2(idiobs),idcat3(idiobs),
     ?xob(idiobs),yob(idiobs),seng(idiobs),altu(idiobs),fgcc(idiobs),
     ?cmag(idiob2),omag(idiobs),cmgj(idiob3),cmgh(idiob3),cmgk(idiob3),
     ?ecmgj(idiob3),ecmgh(idiob3),ecmgk(idiob3),cpma(idiob2),
     ?cpmd(idiob2),cepma(idiob2),cepmd(idiob2),expix(idiobs),
     ?eypix(idiobs),era(idiobs),ede(idiobs),alfres(idiobs),
     ?decres(idiobs),itira(idiobs),ra(idiobs),de(idiobs),xfdp(idiobs),
     ?yfdp(idiobs)


      character*20 ichfil
      character*150 infits
      character*69 mchobj

      character*(kxy) ixy(idiobs),mxy

      character*1 obtype(idiobs)

      character*5 kind(idiobs)


c

      jj=0

      
      do i=1,nest

      xmgu=d99

      pma=d99
      pmd=d99
      epma=d99
      epmd=d99

      xmgj=d99
      xmgh=d99
      xmgk=d99
      ermgj=d99
      ermgh=d99
      ermgk=d99

      ktira=99

      alsi2=d99
      desi2=d99


      xfd=xfdp(i)
      yfd=yfdp(i)

      if (xfd.le.-100.d0 .or. xfd.ge.1000.d0) xfd=d99 
      if (yfd.le.-100.d0 .or. yfd.ge.1000.d0) yfd=d99 




c
c     Retrieves (J,H,K) magnitudes and errors for all 2MASS catalogue stars
c


      if (idcat3(i).ne.0) then

      j=idcat3(i)

      xmgj=cmgj(j)
      xmgh=cmgh(j)
      xmgk=cmgk(j)
      ermgj=ecmgj(j)
      ermgh=ecmgh(j)
      ermgk=ecmgk(j)

      endif


c
c     Retrieves a given catalogue's original magnitude and proper motions
c     information (usually from the Gaia DR3 catalogue)
c


      if (idcat2(i).ne.0) then

      j=idcat2(i)

      xmgu=cmag(j)

      pma=cpma(j)
      pmd=cpmd(j)
      epma=cepma(j)
      epmd=cepmd(j)

      endif


c
c     Retrieves (RA,Dec) reduction information for reference catalogue stars
c

      if (idcat1(i).ne.0) then

      jj=jj+1

      j=idcat1(i)

      alsi2=alfres(i)
      desi2=decres(i)

      ktira=itira(i)

      endif

c


      ras=ra(i)/15.d0

      seeing=scala*seng(i)


      write(mxy,470) xob(i),yob(i),seeing,altu(i),fgcc(i),xfd,yfd,xmgu,
     ?omag(i),oemag,xmgj,xmgh,xmgk,fumag,fumag2,ermgj,ermgh,ermgk,
     ?pma,pmd,epma,epmd,expix(i),eypix(i),era(i),ede(i),alfsig,delsig,
     ?nstart,nfinal,alsi2,desi2,ktira,ras,de(i),iuth,iutm,sut,iutano,
     ?iutmes,iutdia,dj,iexps,ichfil,infits,mchobj,nx,ny,kind(i),
     ?obtype(i)


 470  format(2(1x,f7.2),1x,f5.2,2f10.2,2f7.3,11(1x,f6.3),4(1x,f7.3),
     ?6(1x,f6.3),2(1x,i4),2(1x,f6.3),1x,i4.4,2(1x,f13.9),1x,i2.2,1x,
     ?i2.2,1x,f5.2,1x,i4,1x,i2.2,1x,i2.2,1x,f16.8,2x,i4,2x,a20,2x,a50,
     ?1x,a20,2(1x,i5),1x,a5,1x,a1)



      ixy(i)=''

      ixy(i)=mxy


      enddo


c     close (64)

      return
      end



c
c   
c     Subrotine estat
c
c
c     Outputs target statistics and metadata (mag, epoch, measuring and position
c     errors, etc) on screen and in PRAIA, MPC and NIMA file formats.
c
c     Flushes updated xy astrometry data to PRAIA xy file.
c
c
c
c     Variables:
c
c     ialvos - target file with (RA,Dec)s and instants of targets.
c
c     input  - PRAIA xy file with (RA,Dec) astrometry
c 
c     ixy    - stored xy file data
c
c     ipraia - output target file in PRAIA format
c
c     impc   - output target file in MPC format
c
c     inima  - output target file in NIMA format
c
c
c     nest   - number of measured objects in the FOV
c
c     box    => position error box for target identification in arcseconds.
c
c     kob    - key for reference catalogue object type:
c
c              1 - Gaia DR3 star
c              2 - User catalogue reference object
c              3 - Ephemeris reference object
c
c
c     Object codes:
c
c
c     0    - Reference catalogue object used in (RA,Dec) reductions
c
c     1    - Reference catalogue object not used in (RA,Dec) reductions
c
c     3    - Flagged Gaia DR3 catalogue star, eliminated prior to (RA,Dec)
c            reductions. Flag = no trigonometric parallax available.
c
c    30    - Flagged Gaia DR3 catalogue star, eliminated prior to (RA,Dec)
c            reductions. Flag = no proper motions available.
c
c   300    - Flagged Gaia DR3 catalogue star, eliminated prior to (RA,Dec)
c            reductions. Flag = duplicity.
c
c  3000    - Flagged Gaia DR3 catalogue star, eliminated prior to (RA,Dec)
c            reductions. Flag = outside user-defined G magnitude range.
c
c     7    - Reference catalogue object is also a target
c
c     8    - Target object, not in the reference catalogue
c
c    99    - Unidentified field object
c
c
c    Note 1: There can be any number of combinations for flagged
c            Gaia DR3 stars, like 33, 3033, 3300, etc.
c
c    Note 2: the "+" or "-" sign right before the object code indicates
c            that the object was detected respectively by the PRAIA's
c            BOIA  method or by force after preliminary (RA,Dec) reduction.
c
c
c
c     Last modification:  M. Assafin   19/Jan/2019
c
c

      subroutine estat (idiobs,linxy,nest,ixy,box,nuta,tara,tade,tadist,
     ?tamag,tafase,tarad,tawl,iobalv,input,ipraia,impc,inima,obtipo,
     ?band,iau,icat,kob)


      IMPLICIT REAL *8 (A-H,O-Z)

      dimension tara(idiobs),tade(idiobs),tadist(idiobs),tamag(idiobs),
     ?tafase(idiobs),tarad(idiobs),tawl(idiobs)

      character*150 infits
      character*200 input
      character*50 ialvos,ipraia,impc,inima
      character*20 ichfil,mchobj,iobalv(idiobs),iob(3)
      character*1 isig,obtype

      character*5 kind

      character*(linxy) ixy(idiobs),mxy

      character*7 iform

      character*1  obtipo
      character*1  band
      character*3  iau
      character*1  icat

      character*300  ler


      hmsgms(i,j,a)=i+j/60.d0+a/3600.d0

c
c     Initial data
c

      pi    = 0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c
      id7=7
      id8=8

c


      iob(1)='Gaia_DR3_star'

      iob(2)='User_Catalogue_Obj'

      iob(3)='Solar_System_Object'


c
c     Mount format for PRAIA xy file data flush
c

      iform='(a0000)'

      write (iform(3:6),'(i4.4)') linxy


c

      ipegou=0


c
c     Sets up backspace of files (g77, gfortran, etc)
c

      call backsp (1,nbac,7)

c

 40   format(a50)


c
c     No targets?
c

      ifora=0

      if (nuta.eq.0) go to 250

      ifora=1


c
c     Opens output target files in PRAIA, MPC and NIMA formats
c

      open (2,file=ipraia)
      open (3,file=impc)
      open (9,file=inima)

c

 3    read (2,5,end=6) isig
 5    format(a1)
      go to 3
 6    call backsp (2,nbac,2)

c


 7    read (3,5,end=8) isig
      go to 7
 8    call backsp (2,nbac,3)

c



 9    read (9,5,end=13) isig
      go to 9
 13   call backsp (2,nbac,9)


c
c     Searches targets among measured objects
c

      do 100 j=1,nuta


      do  99 i=1,nest



c
c     Loads xy astrometry data
c

      mxy=''

      mxy=ixy(i)


      read (mxy,10,err=99) xob,yob,seng,altu,fgcc,xfdp,yfdp,
     ?xmgu,cudmg,cudmg2,xmgj,xmgh,xmgk,fumag,fumag2,ermgj,ermgh,
     ?ermgk,pma,pmd,epma,epmd,ex,ey,erau,edeu,alfsiu,delsiu,
     ?nstaru,nfinau,alsiu,desiu,ktirau,ra,de,iuth,iutm,sut,iutano,
     ?iutmes,iutdia,dj,iexps,ichfil,infits,mchobj,nx,ny,kind,obtype


 10   format(2(1x,f7.2),1x,f5.2,2f10.2,2f7.3,11(1x,f6.3),4(1x,f7.3),
     ?6(1x,f6.3),2(1x,i4),2(1x,f6.3),1x,i4,2(1x,f13.9),1x,i2,1x,i2,1x,
     ?f5.2,1x,i4,1x,i2,1x,i2,1x,f16.8,2x,i4,2x,a20,2x,a50,1x,a20,2(1x,
     ?i5),1x,a5,1x,a1)


c
c     Position error box filter
c



      dx=(15.d0*ra-tara(j))*dcos(tade(j)*grarad)*3600.d0
      dy=(de-tade(j))*3600.d0


c
c     Target outside position error box.
c
c     Continue target search with other measured objects
c


      if (dabs(dx).gt.box) go to 99
      if (dabs(dy).gt.box) go to 99



c
c     Current measured object is identified with current target
c

      ipegou=1


c

      if (ipegou.eq.1) then

      write (*,*)

      write (*,*) ' offsets (RA,DE), Sigma(RA,DE),    Ncat,   (x, y) err
     ?or,  UTC  instant and date, exptime,  target                    RA
     ?             DEC                mag'

      write (*,*) '       (arcsec)        (arcsec)                (arcse
     ?c)    h  m   s   year mo dy   (s)                           h  m  
     ? s       dg am  as'

      write (*,*)

      ipegou=2

      endif




c
c     Measured (RA,Dec) of target in hexasegimal notation
c

      raco=ra
      deco=de
      iah=raco
      am=(raco-iah)*60.d0
      iam=am
      sa =(am-iam)*60.d0
      if (deco.lt.0.d0) then
      isig='-'  
      deco=-deco
      else
      isig='+' 
      endif
      idg=deco
      dm=(deco-idg)*60.d0
      idm=dm
      ds=(dm-idm)*60.d0


c
c     Day and fraction of day
c

      dia=hmsgms(iuth,iutm,sut)

      dia=dia/24.d0

      dia=dia+iutdia


c
c     Checks if the target is a reference catalogue object.
c
c     If it is, the registered (dx,dy) offset is that
c     computed from the (RA,Dec) reductions (not the
c     one obtained here in the target identification).
c     The target is then flagged with code "7".
c
c
c     If it is not a reference catalogue object,
c     the (dx,dy) offsets found are registered
c     and the target is flagged with code "8".
c

      if (ktirau.ne.99) then

      ktirau=id7

      else

      ktirau=id8

      alsiu=dx
      desiu=dy

      endif      

c
c     Outputs object/target data on screen
c


      write (*,16) dx,dy,alfsiu,delsiu,nfinau,ex,ey,iuth,iutm,sut,
     ?iutano,iutmes,iutdia,iexps,iobalv(j),iah,iam,sa,isig,idg,idm,
     ?ds,cudmg,band

 16   format(4(1x,f7.3),2x,i5,2x,2(1x,f7.3),2x,i2.2,1x,i2.2,1x,f5.2,1x,
     ?i4,1x,i2.2,1x,i2.2,2x,i4,5x,a20,1x,i2.2,1x,i2.2,1x,f7.4,2x,a1,
     ?i2.2,1x,i2.2,1x,f6.3,9x,f5.2,1x,a1)


c
c     Updates xy file data
c


      mxy=''

      write (mxy,10) xob,yob,seng,altu,fgcc,xfdp,yfdp,
     ?xmgu,cudmg,cudmg2,xmgj,xmgh,xmgk,fumag,fumag2,ermgj,ermgh,
     ?ermgk,pma,pmd,epma,epmd,ex,ey,erau,edeu,alfsiu,delsiu,
     ?nstaru,nfinau,alsiu,desiu,ktirau,ra,de,iuth,iutm,sut,iutano,
     ?iutmes,iutdia,dj,iexps,ichfil,infits,iobalv(j),nx,ny,kind,obtype

      ixy(i)=mxy


c
c     Outputs object/target data on PRAIA output target format
c


      write (2,11) dx,dy,xob,yob,seng,altu,fgcc,xfdp,yfdp,
     ?xmgu,cudmg,cudmg2,xmgj,xmgh,xmgk,fumag,fumag2,ermgj,ermgh,
     ?ermgk,pma,pmd,epma,epmd,ex,ey,erau,edeu,alfsiu,delsiu,
     ?nstaru,nfinau,alsiu,desiu,ktirau,ra,de,iuth,iutm,sut,iutano,
     ?iutmes,iutdia,dj,iexps,ichfil,infits,iobalv(j),nx,ny,kind,obtype


c11   format(2(1x,f7.3),2(1x,f7.2),1x,f5.2,2f10.2,2f7.3,11(1x,f6.3),
c    ?4(1x,f7.3),6(1x,f6.3),2(1x,i4),2(1x,f6.3),1x,i4,2(1x,f13.9),1x,
c    ?i2,1x,i2,1x,f5.2,1x,i4,1x,i2,1x,i2,1x,f16.8,2x,i4,2x,a20,2x,a50,
c    ?1x,a20,2(1x,i5))

 11   format(2(1x,f7.3),2(1x,f7.2),1x,f5.2,2f10.2,2f7.3,11(1x,f6.3),
     ?4(1x,f7.3),6(1x,f6.3),2(1x,i4),2(1x,f6.3),1x,i4,2(1x,f13.9),1x,
     ?i2.2,1x,i2.2,1x,f5.2,1x,i4,1x,i2.2,1x,i2.2,1x,f16.8,2x,i4,2x,a20,
     ?2x,a50,1x,a20,2(1x,i5),1x,a5,1x,a1)



c
c     Outputs object/target data on MPC output target format
c



      ler=''


c     write (ler,26) obname,obtipo,iutano,iutmes,dia,iah,iam,sa,
c    ?isig,idg,idm,ds,cudmg,band,iau,icat

      write (ler,26) iobalv(j),obtipo,iutano,iutmes,dia,iah,iam,sa,
     ?isig,idg,idm,ds,cudmg,band,iau,icat

 26   format(a14,a1,i4.4,1x,i2.2,1x,f8.5,1x,i2.2,1x,i2.2,1x,f6.3,a1,
     ?i2.2,1x,i2.2,1x,f5.2,9x,f5.2,a1,6x,a3,1x,a1)


      if (ler(24:24).eq.' ') ler(24:24)='0'

      if (ler(39:39).eq.' ') ler(39:39)='0'

      if (ler(52:52).eq.' ') ler(52:52)='0'

      if (ler(66:66).eq.' ') ler(66:66)='0'

c

      write (3,27) ler
 27   format(a82)



c
c     Outputs object/target data on NIMA output target format
c



      ler='' 

      write (ler,28) iah,iam,sa,isig,idg,idm,ds,cudmg,dj,iau,icat
 28   format(i2.2,1x,i2.2,1x,f7.4,2x,a1,i2.2,1x,i2.2,1x,f6.3,3x,f6.3,2x,
     ?f16.8,2x,a3,2x,a1)



      if (ler(01:01).eq.' ') ler(01:01)='0'

      if (ler(04:04).eq.' ') ler(04:04)='0'

      if (ler(07:07).eq.' ') ler(07:07)='0'

      if (ler(17:17).eq.' ') ler(17:17)='0'

      if (ler(20:20).eq.' ') ler(20:20)='0'

      if (ler(23:23).eq.' ') ler(23:23)='0'
c

      write (9,29) ler,iobalv(j)
 29   format(a63,3x,a20)


 99   continue

 100  continue


c
c     Finishes up target search 
c


      close (2)
      close (3)
      close (9)

 250  continue


c
c     Updates object types in xy astrometry data
c

      do i=1,nest


      mxy=''

      mxy=ixy(i)

      read(mxy(233:236),*) icode


c
c     Object is a reference catalogue object
c

      if (icode.eq.0 .or. icode.eq.1) then


      mxy(386:405)=''
      mxy(386:405)=iob(kob)

      ixy(i)=mxy


      endif

c
c     Object is a Gaia DR3 flagged star
c

      if (mxy(233:233).eq.'3'.or.mxy(234:234).eq.'3'.or.mxy(235:235).
     ?eq.'3'.or.mxy(236:236).eq.'3') then


      mxy(386:405)=''
      mxy(386:405)='Gaia_DR3_flag_star'

      ixy(i)=mxy


      endif



c
c     Unidentified field object
c

      if (icode.eq.99) then


      mxy(386:405)=''
      mxy(386:405)='Unidentified_Object'

      ixy(i)=mxy


      endif


      enddo



c
c     Flushes updated xy astrometry data to PRAIA xy file
c

      open(1,file=input)

      do i=1,nest

      write (1,iform) ixy(i)

      enddo

      close (1)


c
c     Alerts if no target was found after the search
c

      if (ifora.eq.1) then
      if (ipegou.eq.0) then
      write (*,*) ' Target(s) not identified.'
      endif
      endif

c

      return
      end





c   
c     Subroutine backsp
c
c
c     Executes the correct number of backspaces on an open file "L",
c     to go back one line on file "L".
c      
c
c
c     key=1 : only determines the correct number of backspaces to be done
c
c     key=2 : executes the correct number of backspaces on an open file "L"
c
c     nbac = the correct number of backspaces to be done to go back 1 line
c            in the file
c
c     L    = internal file unity
c
c
c     Last update: M. Assafin 09/Mar/2009
c

      subroutine backsp (key,nbac,L)

      implicit real*8 (a-h,o-z)

      character*20 imaux
      character*4 jmaux
      character*9 sista
      character*29 systa


c
c     Key=2, execute backspace
c


      if (key.eq.2) then

      do i=1,nbac
      backspace L
      enddo

      return

      endif

c
c     Key=1, determines the correct number of backspaces to be done
c     to go back 1 line in the open file
c


      sista='rm -f -r '

      imaux=''

      imaux(1:16)='PRAIA_backsp.aux'


      do 1 i=1,9999

      write (jmaux,'(i4.4)') i

      imaux(17:20)=jmaux(1:4)

      open(L,file=imaux,status='old',err=2)
      close (L)
 1    continue

 2    close (L)

      open(L,file=imaux)

      systa=sista//imaux

c

      do i=1,10
      write (L,*) i
      enddo

      close (L)

      open(L,file=imaux)
      
      do i=1,15
      read (L,*,end=10)
      enddo

 10   do i=1,2
      backspace L
      enddo
      
 
      read (L,*) nbac

      close (L)

      nbac=nbac-9

c


      call system (systa)

      return

      end







c
c
c     Subroutine wfits
c
c     Writes a fits image given a matrix of pixels.
c
c
c     bitpix = 16  integer*2 data
c              +32 integer*4 data
c              -32 real*4 data
c              +64 integer*8 data
c              -64 real*8 data
c
c
c     iswap = 1 (do not swap image)
c             2 (swap image)
c
c
c
c
c     Last modified:   M. Assafin  01/Mar/2022
c

      subroutine wfits (idimx,idimy,if,file,bitpix,iswap,nx,ny,pixel,
     ?bscale,bzero)

      implicit real*8 (a-h,o-z)


      integer*2 iwork2(1440)
      integer*4 iwork4(720)
      integer*8 iwork8(360)
      real*4    rwork4(720)
      real*8    rwork8(360)

      integer*1 swork(2880),iby8(8)
      integer*2 bitpix

      real*4 pixel(idimx,idimy)

      character*50 file
      character*2880 header

      icab(il,ic)=(il-1)*80+ic


c
c     Initial data
c

      nbytes=2880

      if (bitpix.eq.16)  ibytes=2
      if (bitpix.eq.32)  ibytes=4
      if (bitpix.eq.-32) ibytes=4
      if (bitpix.eq.64)  ibytes=8
      if (bitpix.eq.-64) ibytes=8
      

c

      kwork=nbytes/ibytes


c
c     Opens fits file
c 


      open (if,file=file,access='direct',form='unformatted',
     ?recl=2880)



c
c     Writes fits header
c

      header=''

      l=1
      ic1=1
      ic2=46
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='SIMPLE  =                    T / Fits Standard'

      l=2
      ic1=1
      ic2=47
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='BITPIX  =                      / Bits per pixel'

      l=3
      ic1=1
      ic2=47
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='NAXIS   =                    2 / Number of axes'


      l=4
      ic1=1
      ic2=44
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='NAXIS1  =                      / Axis Length'


      l=5
      ic1=1
      ic2=44
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='NAXIS2  =                      / Axis Length'


      l=6
      ic1=1
      ic2=44
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='BSCALE  =                      / Data scale '


      l=7
      ic1=1
      ic2=44
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='BZERO   =                      / Zero point '


      l=2
      ic1=28
      ic2=30
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      write (header(ip1:ip2),'(i3)') bitpix


      l=4
      ic1=26
      ic2=30
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      write (header(ip1:ip2),'(i5)') nx

      l=5
      ic1=26
      ic2=30
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      write (header(ip1:ip2),'(i5)') ny


      l=6
      ic1=11 
      ic2=30
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      write (header(ip1:ip2),'(f20.10)') bscale


      l=7
      ic1=11 
      ic2=30
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      write (header(ip1:ip2),'(f20.10)') bzero 



      do l=8,35
      ic1=1
      ic2=41
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='COMMENTS=                      / Comments'
      enddo

      l=36
      ic1=1
      ic2=3
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='END'

      irec=1

      write (if,rec=irec) header

c
c     Now writes the data
c


      m=0

      do i=1,ny
      do j=1,nx

      m=m+1

c
      if (bitpix.eq.16) then
      iwork2(m)=pixel(j,i)
      if (m.eq.kwork) then
      irec=irec+1
      write (if,rec=irec) iwork2
      endif
      endif
c
      if (bitpix.eq.32) then
      iwork4(m)=pixel(j,i)
      if (m.eq.kwork) then
      irec=irec+1
      write (if,rec=irec) iwork4
      endif
      endif
c
      if (bitpix.eq.64) then
      iwork8(m)=pixel(j,i)
      if (m.eq.kwork) then
      irec=irec+1
      write (if,rec=irec) iwork8
      endif
      endif
c
      if (bitpix.eq.-32) then
      rwork4(m)=pixel(j,i)
      if (m.eq.kwork) then
      irec=irec+1
      write (if,rec=irec) rwork4
      endif
      endif
c
      if (bitpix.eq.-64) then
      rwork8(m)=pixel(j,i)
      if (m.eq.kwork) then
      irec=irec+1
      write (if,rec=irec) rwork8
      endif
      endif
c


      if (m.eq.kwork) then
      m=0
      if (iswap.eq.2) then
      call swapo (if,ibytes,nbytes,irec,swork)
      write (if,rec=irec) swork
      endif
      endif

      enddo
      enddo


      if (m.eq.0) go to 50

      irec=irec+1

      if (bitpix.eq.16) write (if,rec=irec) (iwork2(ii),ii=1,m)
      if (bitpix.eq.32) write (if,rec=irec) (iwork4(ii),ii=1,m)
      if (bitpix.eq.64) write (if,rec=irec) (iwork8(ii),ii=1,m)
      if (bitpix.eq.-32) write (if,rec=irec) (rwork4(ii),ii=1,m)
      if (bitpix.eq.-64) write (if,rec=irec) (rwork8(ii),ii=1,m)

      if (iswap.eq.2) then
      nbytes=m*ibytes
      call swapo (if,ibytes,nbytes,irec,swork)
      write (if,rec=irec) (swork(ii),ii=1,nbytes)
      endif

 50   continue


      close (if)


      return
      end



c
c
c     subroutine swapo
c
c
c     Swap bytes of pixel data
c
c     Image can by integer or floating point
c
c
c     Last modified: M. Assafin   15/Aug/2015
c


      subroutine swapo (if,ibytes,nbytes,irec,swork)

      IMPLICIT REAL *8 (A-H,O-Z)
      
      integer*1 swork(2880),iby8(8)

c

      read (if,rec=irec) swork


      do k=ibytes,nbytes,ibytes

      do m=1,ibytes
      iby8(m)=swork(k-m+1)
      enddo

      do m=1,ibytes
      swork(k-ibytes+m)=iby8(m)
      enddo

      enddo


c

      return
      end





c   
c
c     subroutine desvio
c
c
c     Mean and standard deviation
c
c
c     entry:
c
c        xvam  = sum of individual values
c        xvas  = sum of square of individual values
c
c     output:
c
c        xvam  = mean
c        xvas  = standard deviation about that mean
c
c
c     Last modified: M. Assafin   15/Aug/2015
c

      subroutine desvio (nest,xvam,xvas)

      implicit real *8 (a-h,o-z)

c

      zero=0.d0
      dnove=99.999d0
      dneg=-1.d0

c

      if (nest.le.0) then
      xvam=zero
      xvas=dneg
      return
      endif


c

      exmed=xvam/nest

c

      if (nest.eq.1) then

      xvas=dneg

      return

      endif

c

      if (nest.eq.2) then


      xvas=dsqrt(dabs(2.d0*xvas-xvam*xvam))/2.d0

      xvam=exmed


      return

      endif

c

      raiz=xvas-2.d0*exmed*xvam+nest*exmed*exmed

      if (raiz.lt.0.d0) then

      xvas=dneg

      else

      xvas=dsqrt(raiz/(nest-1.d0))

      endif
c

      xvam=exmed

      return

      end








c
c
c
c     Subroutine magnitudes
c
c
c     Computes PSF-based or aperture-based object magnitudes, based on
c     the furnished volume (PSF) or flux (aperture) of objects.
c
c 
c
c      Last modification: M. Assafin 01/Mar/2022
c
c
c
c

      subroutine magnitudes (d99,idiobs,idigai,nest,percs,ncut,ior1,
     ?nval1,contag,ceu,ceu2,volum,idga1,cg1mgg,ug1mgg,g1smg,g1fmg,
     ?g1fmgs,cutmin,cutmax,itira)


      implicit real *8 (a-h,o-z)


      dimension ior1(idiobs),nval1(idiobs),contag(idiobs),volum(idiobs),
     ?idga1(idiobs),cg1mgg(idigai),ug1mgg(idiobs),itira(idiobs)


c
c     Magnitudes and error in the catalogue system
c

      g1fmg=d99
      g1fmgs=d99
      g1smg=d99

 
      do i=1,nest
      ug1mgg(i)=d99
      enddo

      
c

      zgromg=0.d0
      n=0

      do 10 i=1,nest
      if (idga1(i).eq.0) go to 10
      if (volum(i).le.0.d0) go to 10
      if (itira(i).ne.0) go to 10
      j=idga1(i)
      if (cg1mgg(j).lt.cutmin) go to 10
      if (cg1mgg(j).gt.cutmax) go to 10
      n=n+1
      zgromg=zgromg+cg1mgg(j)+2.5d0*dlog10(volum(i))
 10   continue


      if (n.ge.1) then
      zgromg=zgromg/n
      else
      go to 20
      endif


      n=0

      do 15 i=1,nest

      if (volum(i).le.0.d0) go to 15

      ug1mgg(i)=zgromg-2.5d0*dlog10(volum(i))

      if (idga1(i).eq.0) go to 15
      if (itira(i).ne.0) go to 15
      j=idga1(i)
      if (cg1mgg(j).lt.cutmin) go to 15
      if (cg1mgg(j).gt.cutmax) go to 15
      n=n+1
      contag(n)=ug1mgg(i)-cg1mgg(j)
 15   continue


      if (n.ge.ncut) call quartl (idiobs,ior1,n,percs,contag)

      call avsdev (idiobs,n,contag,g1amg,g1smg)



c
c     Sky background magnitude in the catalogue system
c


      if (ceu.le.0.d0) then

      g1fmg=d99
      g1fmgs=d99

      else

      g1fmg=zgromg-2.5d0*dlog10(ceu)
      g1fmgs=zgromg-2.5d0*dlog10(ceu+ceu2)
      g1fmgs=g1fmg-g1fmgs

      endif


 20   continue

      return
      end









c
c
c     Subroutine mesure
c
c
c     Purpose
c
c
c     User furnishes regions for measurement of specific targets, for each
c     individual image.
c
c    
c
c     Comments
c
c
c     There are two types of targets. The two types are identified by
c     the type of reagion: circle or box.
c
c     In the case of circles, targets have stellar-like PSFs, and are fitted
c     by the circular Gaussian model. 
c
c     In the case of boxes, the targets are trace images, and are fitted
c     by the ERF PSF model.
c    
c
c     These user's regions have higher priority over the automatic object
c     search. This means that no automatic search is applied over the marked
c     regions. The fits are made directly on the pixels inside the furnished
c     regions.
c
c     The format of the regions follow the ds9 package standards. The user
c     should use the cursor facilities of ds9 to produce the regions. 
c
c
c     Last update: M. Assafin   15 Feb 2015
c
c




      subroutine mesure (idin,idiobs,imes,nmcir,xcir,ycir,lacir,bcgc,
     ?bcgsc,nmtra,xtra,ytra,xlatra,ylatra,angtra,bcgtr,bcgstr)

      implicit real*8 (a-h,o-z)
      parameter(idim=150)

      dimension xcir(idiobs),ycir(idiobs),lacir(idiobs),bcgc(idiobs),
     ?bcgsc(idiobs)

      dimension xtra(idiobs),ytra(idiobs),xlatra(idiobs),ylatra(idiobs),
     ?angtra(idiobs),bcgtr(idiobs),bcgstr(idiobs)


      character*(idin) imes
      character*(idim) linha

c

      open (25,file=imes,status='OLD',err=10)

c
c     Searches for targets
c

      nmcir=0
      nmtra=0


c

      do k=1,idiobs

      linha=''
      read (25,5,end=10) linha
 5    format(a150)


c
c     Circle or box ?
c
c     jj=1  ==> circle
c     jj=1  ==> box
c
c     If not circle or box no object is loaded
c


      jj=0

      do j=1,idim-2

      if (linha(j:j+2).eq.'cir') jj=1
      if (linha(j:j+2).eq.'box') jj=2

      enddo


c
c     Only numbers are left
c

      do i=1,idim
      if (linha(i:i).ne.'.') then
      icomp=ichar(linha(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) linha(i:i)=' '
      endif
      enddo


c
c     Circle
c

      if (jj.eq.1) then
      nmcir=nmcir+1
      read (linha,*) xcir(nmcir),ycir(nmcir),raio
      lacir(nmcir)=raio
      endif


c
c     Box (trace images)
c


      if (jj.eq.2) then
      nmtra=nmtra+1
      read (linha,*) xtra(nmtra),ytra(nmtra),dx,dy,ang
      xlatra(nmtra)=dx
      ylatra(nmtra)=dy
      angtra(nmtra)=ang
      endif


      enddo

c

 10   close (25)




      return

      end




c
c
c     Subroutine irot
c
c
c
c     Purpose
c
c
c     Given the internal coordinates (jj,ii) within a rectangle of internal
c     origin zero (0,0) centered at CCD coordinates (ixc,iyc) and rotated by
c     an angle ango in this CCD frame, irot converts the internal coordinates
c     (jj,ii) to the corresponding external CCD coordinates (k,m).
c
c     jj,ii - input internal rectangle coordinates referred to the external
c             rectangle center with CCD coordinates (ixc,iyc), so that in the
c             internal rectangle frame, the origin (jj=0,ii=0) corresponds to
c             (ixc,iyc) in the CCD frame 
c
c     k,m   - output rotated absolute coordinates referred to the external CCD
c             frame  (not referred to the center). They (k,m) coordinates are
c             the actual (x,y) coordinates of the pixel in the CCD frame.
c
c
c                                                               
c                                                       ^        
c        |---------------------------------|          m |     
c        |      .(jj,ii)                   |            |    
c     ii |              .(ixc,iyc)         |   -->      |       .(ixc,iyc)
c        |                                 |            |  
c        |---------------------------------|            |   .(k,m)
c                        jj                             |------------->
c                                                                    k   
c          rectangle associated to the                     CCD
c           trace-shaped object                            frame
c
c
c
c
c     Last update: M. Assafin   03/Nov/2016
c
c
c
      subroutine  irot (jj,ii,ixc,iyc,ango,nx,ny,k,m,iout)

      implicit real*8 (a-h,o-z)

c

      iout=0

c

      ang=-ango

c


      k= jj*dcos(ang)+ii*dsin(ang)
      m=-jj*dsin(ang)+ii*dcos(ang)

      k=k+ixc
      m=m+iyc

      if (k.lt.1) then
      k=1
      iout=1
      endif

      if (m.lt.1) then
      m=1
      iout=1
      endif

      if (k.gt.nx) then
      k=nx
      iout=1
      endif

      if (m.gt.ny) then
      m=ny
      iout=1
      endif

      return
      end






c
c     Subroutine trace 
c
c
c     Purpose
c
c
c     Fits a trace-shaped object with the Error Function PSF model, and finds
c     the (x,y) centroid, among the other parameters of the model, and their errors.
c
c
c     Comments
c
c     The 7 parameters of the ERF model: 
c
c
c    1 - height (counts)
c    2 - x0 (pixels)
c    3 - y0 (pixels)
c    4 - sigma (related to seeing) in pixels
c    5 - tetha angle (in radians)
c    6 - trace lenght (pixels)
c    7 - background (counts)
c
c
c
c     Last update: M. Assafin   05/Sep/2015
c
c
c
c



      subroutine trace (idiobs,idimx,idimy,icofsp,pixmat,imagem,fapitr,
     ?dlimit,plimit,nx,ny,xc,yc,rx,ry,ang,fc,sb,fotceu,lper,contag,ior,
     ?nval,icomax,altura,bx,by,sigma,tetha,dlenght,fundo,ex,ey,ierro,
     ?itx,ity)


      implicit real*8 (a-h,o-z)
      parameter(nterms=7)

      real*4 pixmat(idimx,idimy)

      real*4 pixel1(idimx,idimy),pixel2(idimx,idimy),pixel3(idimx,idimy)

      integer*2 betpix,imagem(idimx,idimy)


      dimension deltax(nterms),xsigma(nterms),param(nterms),
     ?contag(idiobs),ior(idiobs),nval(idiobs)


      character*50 subf


c
c     Initial data
c

      pi    = 0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

      perc=0.2d0


c
c     Parameter increment and lambda factor in Maquard non-linear LS method
c

      dinc=0.05d0
      xlamda=0.001

c
c     Convergence limits
c

      icontx=0

      xresid=-1.d14
      xcent=1.d14
      ycent=1.d14

      residx=0.d0


c
c     Trace center, sides and orientation angle
c


      ixc=xc
      iyc=yc
      irx=rx/2.d0
      iry=ry/2.d0

      rang=ang

c
c     Estimated initial values for the parameters
c


c
c     Sky background
c 


      param(7)=fc


c
c     (x, y) center
c


      param(2)=xc
      param(3)=yc



c
c     Tetha
c

      param(5)=rang



c
c     Amplitude at center (auxiliary parameters)
c
c     Not the height parameter A of the model
c



      kk=lper

c     if (kk.lt.5) kk=5

      n=0

      ii=0

      do 05 jj=-kk,kk

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 05

      if (imagem(j,i).lt.-19) go to 05

      n=n+1
      contag(n)=pixmat(j,i)


  05  continue



      call quartl (idiobs,ior,n,perc,contag)


      call avsdev (idiobs,n,contag,h,aux)


      hh=h-fc

      h2=hh/2.d0



c
c     Sigma
c
c     Estimated from the FWHM along the smaller width of the trace at the trace
c     center
c
c


      i1=0
      i2=0


      do 11 ii=0,-iry,-1

      n=0

      do 10 jj=-kk,kk

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 10

      if (imagem(j,i).lt.-19) go to 10

      n=n+1
      contag(n)=pixmat(j,i)

  10  continue


      call quartl (idiobs,ior,n,perc,contag)

      if (n.lt.1) go to 11

      call avsdev (idiobs,n,contag,hhh,aux)


      if ((hhh-fc).lt.h2) then
      i1=-ii
      go to 12
      endif


  11  continue

c

  12  continue


      do 14 ii=0,iry

      n=0

      do 13 jj=-kk,kk

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 13

      if (imagem(j,i).lt.-19) go to 13

      n=n+1
      contag(n)=pixmat(j,i)

  13  continue


      call quartl (idiobs,ior,n,perc,contag)

      if (n.lt.1) go to 14

      call avsdev (idiobs,n,contag,hhh,aux)


      if ((hhh-fc).lt.h2) then
      i2=ii
      go to 15
      endif


  14  continue

c

  15  continue


      fwhms=i1+i2+1
      sigma=fwhms/(2.d0*1.177410023d0)

      param(4)=sigma





c
c     Lenght parameter d
c     
c     Estimated from the FWHM along the trace lenght
c
c


      j1=0
      j2=0

      ii=0

      do 30 jjj=0,-irx,-kk-1

      n=0

      do 20 jj=jjj,jjj-kk,-1

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 20

      if (imagem(j,i).lt.-19) go to 20

      n=n+1
      contag(n)=pixmat(j,i)

  20  continue


      call quartl (idiobs,ior,n,perc,contag)

      if (n.lt.1) go to 30

      call avsdev (idiobs,n,contag,hhh,aux)


      if ((hhh-fc).lt.h2) then
      j1=-jjj+kk/2
      go to 35
      endif


 30   continue

c

 35   continue


      do 50 jjj=0,irx,kk+1

      n=0

      do 40 jj=jjj,jjj+kk

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 40

      if (imagem(j,i).lt.-19) go to 40

      n=n+1
      contag(n)=pixmat(j,i)

  40  continue


      call quartl (idiobs,ior,n,perc,contag)

      if (n.lt.1) go to 50

      call avsdev (idiobs,n,contag,hhh,aux)



      if ((hhh-fc).lt.h2) then
      j2=jjj-kk/2
      go to 55
      endif


 50   continue

c

 55   continue

      fwhml=j2+j1+1

      param(6)=fwhml

c     param(6)=214
c     param(1)=3156
c     param(7)=100 



c
c     Height parameter of the model
c


      param(1)=1.d0


      n=0

      ii=0

      do 60 jj=-kk,kk

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 60

      if (imagem(j,i).lt.-19) go to 60

      n=n+1
      contag(n)=(pixmat(j,i)-param(7))/(track(nterms,j,i,param,ierro)-
     ?param(7))


  60  continue


      call quartl (idiobs,ior,n,perc,contag)

      call avsdev (idiobs,n,contag,h,aux)


      param(1)=h




c

      write (*,*)

c     write (*,*) 'irx, iry (pixels) = ',irx, iry
c     write (*,*)

      write (*,*) 'Initial guess:'
      write (*,*) 'height (A) (ADUs) = ',param(1)
      write (*,*) 'xc (pixels)       = ',param(2)
      write (*,*) 'yc (pixels)       = ',param(3)
      write (*,*) 'sigma (pixels)    = ',param(4)
      write (*,*) 'theta (rad)       = ',param(5)
      write (*,*) 'lenght (d) pixels = ',param(6)
      write (*,*) 'sky counts (ADUs) = ',param(7)
      write (*,*)
c     stop



c
c     Initial increments for the parameters
c

c140  continue

      do i=1,nterms
      deltax(i)=dinc*param(i)
      enddo

c
c     Initializing error parameters            
c

      do i=1,nterms
      xsigma(i)=0.d0
      enddo

      
c
c     Trace fitting
c

c     go to 150


 140  call traco (idimx,idimy,icofsp,pixmat,imagem,nx,ny,nterms,param,
     ?deltax,xsigma,xlamda,ixc,iyc,irx,iry,rang,residx,ierro)

c     write (*,*) 'ierro = ',ierro

      if (ierro.eq.1) then
      icontx = icontx + 1
      if (icontx.eq.icomax) go to 150
      xlamda=0.001
      go to 140
      endif      

c
c     Solution convergence
c

      residx = dsqrt(residx)
      centx  = param(2) 
      centy  = param(3) 
      conver = dabs(residx*dlimit)
      diferd = dabs(residx-xresid)
      difpox = dabs(centx-xcent)
      difpoy = dabs(centy-ycent)


c     if ((diferd.lt.conver).and.(difpox.lt.plimit).and.(difpoy.lt.
c    ?plimit)) go to 150

      xresid = residx
      xcent=centx
      ycent=centy
      icontx = icontx + 1
      if (icontx.eq.icomax) go to 150

      go to 140


 150  continue




c
c     Eliminates pixels with discrepant counts by a factor of the sigma
c     of the PSF fitting, one by one, until no pixel is above the threshold
c

      ixc=centx
      iyc=centy
      rang=param(5)

      teto=fapitr*residx

      difmax=-1.d14

      n=0

      do 170 ii=-iry,iry
      do 160 jj=-irx,irx

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 160

      if (imagem(j,i).lt.-9) go to 160

      difcon=dabs(pixmat(j,i)-track(nterms,j,i,param,ierro))

      if (difcon.gt.teto) then

c     write (*,*) 'fapitr,x,y ',difcon/residx,centx,centy

      if (difcon.gt.difmax) then
      difmax=difcon
      n=n+1
      jxx=j
      iyy=i
      endif

      endif

 160  continue
 170  continue

    
      if (n.gt.0) then
      imagem(jxx,iyy)=-10
      icontx=1
      go to 140
      endif



c
c     Resets eliminated pixels with discrepant counts by a factor of the sigma
c     of the PSF fitting of the last iteration
c

 

      do 190 ii=-iry,iry
      do 180 jj=-irx,irx

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 180

      if (imagem(j,i).eq.-10) imagem(j,i)=0

 180  continue
 190  continue



c
c     Storing parameters for the main program
c

      residx = residx
      altura = param(1)
      bx     = param(2) 
      by     = param(3) 
      sigma  = param(4)
      tetha  = param(5)
      dlenght= param(6) 
      fundo  = param(7)


      ex=residx*xsigma(2)
      ey=residx*xsigma(3)



      write (*,*)
      write (*,*) 'Fitted values:'
      write (*,*) 'height (A) (ADUs) = ',param(1)
      write (*,*) 'xc (pixels)       = ',param(2)
      write (*,*) 'yc (pixels)       = ',param(3)
      write (*,*) 'sigma (pixels)    = ',param(4)
      write (*,*) 'theta (rad)       = ',param(5)
      write (*,*) 'lenght (d) pixels = ',param(6)
      write (*,*) 'sky counts (ADUs) = ',param(7)
      write (*,*)
c     stop




c
c     Stores trace images in sub-frames in fits format for debug purposes
c

      if (ixc.ne.itx .and. iyc.ne.ity) return

c

      do i=1,ny
      do j=1,nx
      pixel1(j,i)=0
      pixel2(j,i)=0
      pixel3(j,i)=0
      enddo
      enddo

      ang=tetha

      if=83
      betpix=-32
      mswap=2
      scale=1.d0
      zero=0.d0


      irx=irx+100

      idimx=max0(irx,iry)
 
      idimx=2*idimx

      idimx=idimx+1


      idimy=idimx


      ixcc=idimx/2
      iycc=idimy/2


      do  ii=-iry,+iry
      do  jj=-irx,+irx

      call irot (jj,ii,ixc,iyc,ang,nx,ny,j,i,iout)

      p=pixmat(j,i)

      tt=track(nterms,j,i,param,ierro)

      dd=pixmat(j,i)-tt

c     jjj=j-ixc+ixcc
c     iii=i-iyc+iycc


      call irot (jj,ii,ixcc,iycc,ang,idimx,idimy,j,i,iout)

      if (iout.eq.0) then
      pixel1(j,i)=p
      pixel2(j,i)=tt
      pixel3(j,i)=dd
      endif
 
      enddo
      enddo
 
c
c     Original sub-frame
c

      subf='t_original.fits'

      call wfits (idimx,idimy,if,subf,betpix,mswap,idimx,idimy,pixel1,
     ?scale,zero)


c
c     ERF PSF Model sub-frame
c

      subf='t_traco.fits'


      call wfits (idimx,idimy,if,subf,betpix,mswap,idimx,idimy,pixel2,
     ?scale,zero)


c
c     Original minus ERF PSF Model sub-frame
c

      subf='t_diferenca.fits'

      call wfits (idimx,idimy,if,subf,betpix,mswap,idimx,idimy,pixel3,
     ?scale,zero)


c     stop

      return
      end





c
c     Subroutine traco
c
c
c     Purpose
c
c
c     The trace fit itself by the ERF PSF model, following the Marquardt
c     method of non-linear LS.
c
c
c
c     Comments
c
c
c     The 7 parameters of the ERF PSF model: 
c
c
c    1 - height (counts)
c    2 - x0 (pixels)
c    3 - y0 (pixels)
c    4 - sigma (related to seeing) in pixels
c    5 - tetha angle (in radians)
c    6 - trace width (pixels)
c    7 - background (counts)
c
c
c
c    Subroutines required;
c
c
c    track
c    tqquad
c    tderiv
c    matinv
c
c
c
c     Last update: M. Assafin   21 Jan 2013
c
c
c
c

      subroutine traco (idimx,idimy,icofsp,pixmat,imagem,nx,ny,nterms,a,
     ?deltaa,sigmaa,flamda,ixc,iyc,irx,iry,ang,chisqr,ierro)


      implicit real *8 (a-h,o-z)

      dimension a(nterms),deltaa(nterms),sigmaa(nterms),b(nterms),
     ?alpha(nterms,nterms),beta(nterms),deriv(nterms),
     ?array(icofsp,icofsp)

      real*4 pixmat(idimx,idimy)

      integer*2 imagem(idimx,idimy)


c
c     Initial values
c

      crash=1.d-14

      ierro=0
      det=1.d0
      icont=0
      iconv=20
      jconv=0

c

c
c        Evaluate alpha and beta matrices
c

      do j=1, nterms
      beta(j) = 0.d0
      do k=1, j
      alpha(j,k) = 0.d0
      enddo
      enddo

      do 150  ii=-iry,+iry
      do 5001 jj=-irx,+irx

      call irot (jj,ii,ixc,iyc,ang,nx,ny,l,i,iout)

      if (imagem(l,i).lt.-19) go to 5001
      call tderiv (nterms, l, i, a, deltaa, deriv,ierro)
      if (ierro.eq.1) go to 107
      icont=icont+1

      do j=1,nterms
      beta(j)=beta(j)+(pixmat(l,i)-track(nterms,l,i,a,ierro))*deriv(j)
      if (ierro.eq.1) go to 107
      do k=1, j
      alpha(j,k) = alpha(j,k) + deriv(j)*deriv(k)
      enddo
      enddo

 5001 continue
  150 continue



c
c     Number of degrees of freedom
c

      free=icont-nterms

c

      if (free.le.0.d0) go to 107


c

      do j=1,nterms
      do k=1, j
      alpha(k,j)=alpha(j,k)
      enddo
      enddo


c
c        Evaluates chi square at starting point
c

      chisq1=tqquad(idimx,idimy,nterms,pixmat,imagem,nx,ny,ixc,iyc,irx,
     ?iry,ang,free,a,ierro)


      if (ierro.eq.1) go to 107




c 				 
c         Invert modified curvature matrix to find the new parameters
c


 71   do j=1, nterms
      do k=1, nterms
      aux = alpha(j,j)*alpha(k,k)
      if (dabs(aux).lt.crash) go to 107
      array(j,k)= alpha(j,k) / dsqrt (aux)
      enddo
      array(j,j) = 1.d0 + flamda
      enddo
   80 call matinv (nterms, icofsp, array, det,ierro)
c

      if (ierro.eq.1) go to 107


C
      do j=1, nterms
      b(j) = a(j)
      do k=1, nterms
      aux = alpha(j,j)*alpha(k,k)
      if (dabs(aux).lt.crash) go to 107
      b(j) = b(j) + beta(k)*array(j,k)/dsqrt(aux)
      enddo
      enddo


c
c        If chi square increased, increase flamda and try again
c

      chisqr=tqquad(idimx,idimy,nterms,pixmat,imagem,nx,ny,ixc,iyc,irx,
     ?iry,ang,free,b,ierro)


      if (ierro.eq.1) go to 107

c
c     Convergence to minimum is not being reached
c

      jconv=jconv+1

      if (jconv.gt.iconv) go to 107


c

      if ((chisq1 - chisqr).ge.0.d0) go to 101

      flamda = 10.d0*flamda

      go to 71


c
c        Evaluate parameters and uncertainties
c

  101 do j=1, nterms

      a(j) = b(j)

      if (dabs(alpha(j,j)).lt.crash) go to 107

      aux = array(j,j)/alpha(j,j)

      sigmaa(j) = dsqrt(aux)

      enddo

      flamda = flamda/10.d0

      go to 110

  107 chisqr = -1.d0
      ierro=1

  110 continue



      return
      end


c
c
c     Subroutine tderiv
c
c
c     Evaluates derivatives of the ERF PSF model function for non-linear LS.
c
c
c     The 7 parameters of the ERF PSF model: 
c
c
c    1 - height (counts)
c    2 - x0 (pixels)
c    3 - y0 (pixels)
c    4 - sigma (related to seeing) in pixels
c    5 - tetha angle (in radians)
c    6 - trace width (pixels)
c    7 - background (counts)
c
c
c
c     Last update: M. Assafin   09 Dec 2012
c
c
c
c

      subroutine tderiv (nterms, j, i, a, deltaa, deriv, ierro)

      implicit real*8 (a-h,o-z)

      dimension a(nterms), deltaa(nterms), deriv(nterms)


c

      if (a(4).le.0.d0) then
      ierro=1
      return
      endif 

c

      pi=0.3141592653589793d1

      x=j
      y=i



c
c     Auxiliary terms
c

      s=dsin(a(5))
      c=dcos(a(5))

      z=(-(x-a(2))*s+(y-a(3))*c)/(a(4)*dsqrt(2.d0))
      z2=z*z
 

      w1=((x-a(2))*c+(y-a(3))*s+(a(6)/2.d0))/(a(4)*dsqrt(2.d0))
      w2=((x-a(2))*c+(y-a(3))*s-(a(6)/2.d0))/(a(4)*dsqrt(2.d0))

      w12=w1*w1
      w22=w2*w2

      dk=((x-a(2))*c+(y-a(3))*s)/(a(4)*dsqrt(2.d0))

      efw1=erf(w1)
      efw2=erf(w2)

      dew12=dexp(-w12)
      dew22=dexp(-w22)

      dez2=dexp(-z2)

      sqpi=dsqrt(pi)
      sqpi2=dsqrt(pi/2.d0)


c
c    The derivatives of the ERF PSF model
c


      deriv(1)=sqpi2*a(4)*dez2*(efw1-efw2)

      deriv(2)=-a(1)*dez2*(sqpi*z*s*(efw1-efw2)+c*(dew12-dew22))

      deriv(3)=-a(1)*dez2*(-sqpi*z*c*(efw1-efw2)+s*(dew12-dew22))

      deriv(4)=a(1)*sqpi2*dez2*((1.d0+2.d0*z2)*(efw1-efw2)+(2.d0/sqpi)*
     ?(w1*dew12-w2*dew22))

      deriv(5)=a(1)*2.d0*sqpi2*a(4)*z*dez2*(dk*(efw1-efw2)+(2.d0/sqpi)*
     ?(dew12-dew22))

      deriv(6)=-0.5d0*a(1)*dez2*(dew12+dew22)

      deriv(7)=1.d0


      return

      end




c
c
c     Function  track
c
c
c     The ERF PSF model function for fitting trace-shaped images.
c
c
c     The 7 parameters of the ERF PSF model: 
c
c
c    1 - height (counts)
c    2 - x0 (pixels)
c    3 - y0 (pixels)
c    4 - sigma (related to seeing) in pixels
c    5 - tetha angle (in radians)
c    6 - trace width (pixels)
c    7 - background (counts)
c
c
c
c     Last update: M. Assafin   09 Dec 2012
c
c
c
c

      double precision function track (nterms,j,i,a,ierro)

      implicit real*8 (a-h,o-z)

      dimension a(nterms)


c

      if (a(1).le.0.d0) then
      ierro=1
      return
      endif 

      if (a(4).le.0.d0) then
      ierro=1
      return
      endif 

      if (a(6).lt.0.d0) then
      ierro=1
      return
      endif 





      ierro=0

c

      pi=0.3141592653589793d1

c



      x=j
      y=i



c
c     Auxiliary terms
c

      s=dsin(a(5))
      c=dcos(a(5))

      z=(-(x-a(2))*s+(y-a(3))*c)/(a(4)*dsqrt(2.d0))
      z2=z*z
      dez2=dexp(-z2)
 

      w1=((x-a(2))*c+(y-a(3))*s+(a(6)/2.d0))/(a(4)*dsqrt(2.d0))
      w2=((x-a(2))*c+(y-a(3))*s-(a(6)/2.d0))/(a(4)*dsqrt(2.d0))


      efw1=erf(w1)
      efw2=erf(w2)


      sqpi2=dsqrt(pi/2.d0)


      track=a(1)*sqpi2*a(4)*dez2*(efw1-efw2)+a(7)

c     write (*,*) 'j,i = ',j,i
c     write (*,*) 'z,w1,w2 = ',z,w1,w2
c     write (*,*) 'sqpi2,dez2,efw1,efw2 = ',sqpi2,dez2,efw1,efw2
c     write (*,*) 'track = ',track
c     stop




      return
      end






c
c
c     Function tqquad
c
c
c     Evaluate the reduced chi square for fit to the data
c
c
c
c     Last update: M. Assafin   21 Jan 2013
c
c
c
c

      double precision function tqquad (idimx,idimy,nterms,pixmat,
     ?imagem,nx,ny,ixc,iyc,irx,iry,ang,free,a,ierro)

      implicit real*8 (a-h,o-z)

      real*4 pixmat(idimx,idimy)

      integer*2 imagem(idimx,idimy)

      dimension a(nterms)



c
      chisq = 0.d0
      tqquad=0.d0
c
      if (free.le.0.d0) then
      ierro=1
      return
      endif
c


      do 39 ii=-iry,+iry
      do 38 jj=-irx,+irx

      call irot (jj,ii,ixc,iyc,ang,nx,ny,j,i,iout)

      if (imagem(j,i).lt.-19) go to 38
      chisq = chisq + (pixmat(j,i)-track(nterms,j,i,a,ierro))**2
      if (ierro.eq.1) return
 38   continue
 39   continue


c
c     Reduced chi square: divide by the number of degrees of freedom
c

      tqquad = chisq / free

      return
      end



c
c     Error function 
c


      double precision function erf(x)
      implicit real*8 (a-h,o-z)
c     real*8 erf,x
c     real*8 gammp
      if(x.lt.0.d0)then
        erf=-gammp(.5d0,x**2)
      else
        erf=gammp(.5d0,x**2)
      endif
      return
      end



      double precision function gammp(a,x)
      implicit real*8 (a-h,o-z)
c     real*8 a,gammp,x
c     real*8 gammcf,gamser,gln
c     if(x.lt.0.d0.or.a.le.0.d0) stop 'bad arguments in gammp'
      if(x.lt.a+1.d0)then
        call gser(gamser,a,x,gln)
        gammp=gamser
      else
        call gcf(gammcf,a,x,gln)
        gammp=1.d0-gammcf
      endif
      return
      end




      subroutine gcf(gammcf,a,x,gln)
      implicit real*8 (a-h,o-z)
c     integer*4 ITMAX
c     real*8 a,gammcf,gln,x,EPS,FPMIN
      parameter (ITMAX=100,EPS=3.d-7,FPMIN=1.d-30)
c     integer*4 i
c     real*8 an,b,c,d,del,h,gammln
      gln=gammln(a)
      b=x+1.d0-a
      c=1.d0/FPMIN
      d=1.d0/b
      h=d
      do 11 i=1,ITMAX
        an=-i*(i-a)
        b=b+2.d0
        d=an*d+b
        if(dabs(d).lt.FPMIN)d=FPMIN
        c=b+an/c
        if(dabs(c).lt.FPMIN)c=FPMIN
        d=1.d0/d
        del=d*c
        h=h*del
        if(dabs(del-1.d0).lt.EPS)goto 1
11    continue
c     stop 'a too large, ITMAX too small in gcf'
1     gammcf=dexp(-x+a*dlog(x)-gln)*h
      return
      end





      subroutine gser(gamser,a,x,gln)
      implicit real*8 (a-h,o-z)
c     integer*4 ITMAX
c     real*8 a,gamser,gln,x,EPS
      parameter (ITMAX=100,EPS=3.d-7)
c     integer*4 n
c     real*8 ap,del,sum,gammln
      gln=gammln(a)
      if(x.le.0.d0)then
        if(x.lt.0.d0)stop 'x < 0 in gser'
        gamser=0.d0
        return
      endif
      ap=a
      sum=1.d0/a
      del=sum
      do 11 n=1,ITMAX
        ap=ap+1.d0
        del=del*x/ap
        sum=sum+del
        if(dabs(del).lt.dabs(sum)*EPS)goto 1
11    continue
c     stop 'a too large, ITMAX too small in gser'
1     gamser=sum*dexp(-x+a*dlog(x)-gln)
      return
      end




c
c
c     Subroutine ident 
c
c
c     Identify candidate objects in the FOV above the object-local sky background.
c
c
c     Identifies candidate rounded-shaped objects in the FOV.
c
c     If candidate trace-shaped objects are present, all the trace or parts of it are
c     also detected. 
c
c     If blended objects are present, they are also detected (not functional in this
c     vesion).
c
c     The candidate source regions are analysed in terms of the associated
c     excentricity and orientation angle. The results are stored for posterior
c     usage.
c
c     The excentricity indicates if the candidate source region is a single
c     rounded shaped object, blended sources, or part (or all) of a trace
c     shaped object.
c
c
c
c     Last modified: M. Assafin 19/Jan/2019
c
c
c


      subroutine ident (idimx,idimy,idiobs,icofsp,coefx,pixmat,imagem,
     ?nx,ny,ior,xid,yid,idx1,idx2,idy1,idy2,idlado,npix,bcg,bcgs,nbcg,
     ?dan,exce,sige,snr,cf,cra,cann,uu20,uu02,uu11,seg1,seg2,nest,
     ?snrmin,gain,sv,ixb,iyb,qua,gramal,marca,mx,my,nn,iddo)

      implicit real *8 (a-h,o-z)

      real*4 pixmat(idimx,idimy)
      integer*2 imagem(idimx,idimy)

      dimension sv(idiobs),sv1(idiobs),sv2(idiobs),ior(idiobs),
     ?coefx(icofsp),ixb(idiobs),iyb(idiobs)

      dimension snr(idiobs),cra(idiobs),cann(idiobs),cwid(idiobs),
     ?apix(idiobs),cf(idiobs),marca(idiobs)


      dimension xid(idiobs),yid(idiobs),idlado(idiobs),idx1(idiobs),
     ?idx2(idiobs),idy1(idiobs),idy2(idiobs),npix(idiobs),nbcg(idiobs),
     ?bcg(idiobs),bcgs(idiobs),dan(idiobs),exce(idiobs),sige(idiobs),
     ?uu20(idiobs),uu02(idiobs),uu11(idiobs),seg1(idiobs),seg2(idiobs)


      dimension mx(iddo),my(iddo)


c
c     Initial values
c

      pi=3.141592653589793D0
      grarad=pi/180.d0
      radgra=180.d0/pi

      gwi=1.d0


c
c     Identification of objects by the PRAIA's Browsing Objects:
c     Identification and Analysis (BOIA) procedures
c


      call locate (idimx,idimy,idiobs,sv,ixb,iyb,ior,pixmat,imagem,nx,
     ?ny,gain,xid,yid,cra,cann,cwid,snr,apix,nest,sv1,sv2,snrmin,cf,
     ?nbcg,bcg,bcgs,qua,gramal,marca,mx,my,nn,iddo)


c
c     Final astrometric aperture center
c
c     Refines the aperture center by using the novel centering
c     PRAIA's algorithm Photogravicenter Method (PGM).
c
c     The usual modified moment method only weights the pixel
c     coordinates by the pixel counts. Only pixels above a given
c     threshold count are used.
c
c     In the PGM, besides the counts, the pixels are also weighted
c     according to the (square) distance to the other valid pixels. As in
c     the traditional method, again only pixels above a given threshold count
c     are used.
c
c     The advantage of the PGM over the traditional method is that
c     it is more sensitive to the grouping of real object pixels, for which
c     weights are higher than for the more isolated spurious bright pixels
c     nearby. In this way, the center is improved, particularly for faint
c     objects.
c
c
c     The center refinement with the PGM method is repeated 5 times in order
c     to guarantee that the correct center has been obtained.
c


      write (*,*)
      write (*,*)
      write (*,*) 'Final refinement of astrometric (x,y) aperture'
      write (*,*) 'centers with the PRAIA Photogravicenter Method (PGM) 
     ?...'
      write (*,*)
      write (*,*)


c


      DO kk=1,5
c     DO kk=1,1

      DO k=1,nest

      xi=xid(k)
      yi=yid(k)

      jx1=xi-cra(k)-1
      jx2=xi+cra(k)+1
 
      jy1=yi-cra(k)-1
      jy2=yi+cra(k)+1
 
      if (jx1.lt.1) jx1=1
      if (jy1.lt.1) jy1=1
 
      if (jx2.gt.nx) jx2=nx
      if (jy2.gt.ny) jy2=ny


      call subar (idiobs,idimx,idimy,sv,ior,ixb,iyb,pixmat,nx,ny,xi,yi,
     ?cra(k),jx1,jx2,jy1,jy2,xgb,ygb,nnbb)
 
      xid(k)=xgb
      yid(k)=ygb

      idx1(k)=xid(k)-cra(k)-1
      idx2(k)=xid(k)+cra(k)+1
 
      idy1(k)=yid(k)-cra(k)-1
      idy2(k)=yid(k)+cra(k)+1
 
      if (idx1(k).lt.1) idx1(k)=1
      if (idy1(k).lt.1) idy1(k)=1
 
      if (idx2(k).gt.nx) idx2(k)=nx
      if (idy2(k).gt.ny) idy2(k)=ny


      ENDDO

      ENDDO


      write (*,*)
      write (*,*)
      write (*,*) 'Final refinement of astrometric (x,y) aperture'
      write (*,*) 'centers with the PRAIA Photogravicenter Method (PGM)'
      write (*,*) 'finished.'
      write (*,*)
      write (*,*)





c
c     Eliminates remaining multiple detections of the same object or
c     part of it after final recentering.
c
c     Here, multiple detections are identified when their aperture
c     centers fall inside the aperture of another detection.
c
c     The detection with the largest aperture is preserved.
c


      m=nest

      do k=1,m
      marca(k)=0
      enddo

      do     k=1,m-1
      do 110 j=k+1,m


      d=dsqrt((xid(j)-xid(k))**2+(yid(j)-yid(k))**2)

      if (d.gt.cra(k).and.d.gt.cra(j)) go to 110

c     if (d.gt.(cra(k)+cra(j))) go to 110


c     if (snr(j).lt.snr(k)) then
      if (cra(j).lt.cra(k)) then
      marca(j)=1
      else
      marca(k)=1
      endif

 110  continue
      enddo

      n=0
      
      do 115 k=1,m

         
      if (marca(k).ne.0) go to 115

      n=n+1

      marca(n)=0


      idx1(n)=idx1(k)
      idx2(n)=idx2(k)
      idy1(n)=idy1(k)
      idy2(n)=idy2(k)

      xid(n)=xid(k)
      yid(n)=yid(k)
      cra(n)=cra(k)
      cann(n)=cann(k)
      snr(n)=snr(k)

      cwid(n)=cwid(k)

      apix(n)=apix(k)
      cf(n)=cf(k)
      nbcg(n)=nbcg(k)
      bcg(n)=bcg(k)
      bcgs(n)=bcgs(k)

      
 115  continue


      nest=n



c
c     What is the nature of the objects found?
c
c     For that, we compute the second moments of the image, from which we can
c     derive informations about the object's shape, like its semi-axes,
c     excentricity and orientation angle. These quantities are stored for
c     further use.
c
c
c     Second moments are useful as input in the computation of initial
c     parameters in the PSF fit with circular or elliptical Gaussians and
c     Lorentzians. And useful to estimate the FWHM and (x,y) errors in
c     the (x,y) measurements with the PGM.
c
c
c     From the computed excentricity, we have 3 possibilities:
c
c          a) if the excentricity e is smaller than e1, it is a single rounded
c             shaped object.
c
c          b) if the excentricity e is between  e1 <  e  < e2, it is possible that
c             we have two nearby rounded sources together. A specific procedure
c             must be called to measure the two blended (but rounded) objects.
c             (It has not been implemented in PRAIA yet). 
c
c          c) if the excentricity e is higher than e2, a trace-shaped object, or
c             part of the trace, is likely to have been detected. A specific
c             procedure is called in the sequence of the main program to finish
c             identifying the rectangular region around the trace shaped object.
c
c
c      For posterior analysis, the excentricity and the orientation angle are stored.
c
c


      write (*,*)
      write (*,*)
      write (*,*) 'Computing object moments and excentricity for posteri
     ?or usage in the (x,y) measurements ...'
      write (*,*)
      write (*,*)


      do 80 k=1,nest

      call momeci (idimx,idimy,icofsp,coefx,pixmat,idx1(k),idx2(k),
     ?idy1(k),idy2(k),xid(k),yid(k),cra(k),uu20(k),uu11(k),uu02(k),
     ?seg1(k),seg2(k),sige(k),dan(k),exce(k))



c     write (88,*) 'xc,yc,exc,ang,idlado,chisq,fwhm = ',xid(k),yid(k),
c    ?exce(k),tet*radgra,idlado(k),chisqx,sige(k)*2.35d0/2.d0


      npix(k)=apix(k)

      idlado(k)=cra(k)


c     xlado=idlado(k)
c     write (55,55) xid(k),yid(k),xlado
c55   format('circle(',2(f8.2,','),f8.2,')')




 80   continue


c

      write (*,*)
      write (*,*)
      write (*,*) 'Moments/excentricity computations finished.'
      write (*,*)
      write (*,*)

c     stop

      return

      end



c
c
c
c     Subroutine locate
c
c
c     Purpose
c
c
c     Finds objects in an image. Sets the best (x,y) center, circular aperture radius
c     and circular annulus radius and width for each object, based on the
c     best S/N ratio, following the PRAIA's BOIA procedure.
c
c
c     Main variables
c
c
c     idim,ida   - vector dimensions
c
c     sv,ior     - auxiliary variables
c
c     nx,ny      - matrix dimensions
c     xc,yc      - (x,y) center of each found object
c     cra        - output circular aperture radius for optimal S/N ratio of each object
c     cann       - output circular annulus radius for optimal S/N ratio of each object
c     cwid       - output circular annulus width for optimal S/N ratio of each object
c     snr        - output S/N ratio of object 
c     gwi        - input maximum allowed annulus width
c     sper       - sky background average (quartile statistics)
c     sper       - sky background standard deviation (quartile statistics)
c     ng         - effective number of pixels within circular aperture
c     nbg        - effective number of pixels within circular annulus
c     snrat      - optimal derived S/N ratio
c     npix       - number of pixels within the circular aperture
c     qua        - percentage of elimination of outliers in quartile
c                  statistics of rings
c     gramal     - maximum aperture radius size limited by the dimension
c                  of variables (ida)
c     marca      - flag for false or bad measurements
c     nn         - NNxNN cell size for scanning the matrix
c
c
c
c     gain       - CCD gain
c
c     markc      - astrometric error flag:
c                  0 = (x,y) was computed/updated successfully
c                  1 = (x,y) could not be computed/updated; in this case,
c                      the original (x,y) input is preserved and passed
c                      away
c
c
c     Auxiliary subroutines
c
c     centob
c
c
c
c     Last update:   M. Assafin - 02/Oct/2022
c
c



      subroutine locate (idimx,idimy,ida,sv,ixb,iyb,ior,pixmat,imagem,
     ?nx,ny,gain,xc,yc,cra,cann,cwid,snr,apix,m,sv1,sv2,snrmin,cf,nbcg,
     ?bcg,bcgs,qua,gramal,marca,mx,my,nn,iddo)



      IMPLICIT real*8 (A-H,O-Z)

      real*4 pixmat(idimx,idimy),matpix(idimx,idimy),matpit(idimx,idimy)
      integer*2 imagem(idimx,idimy)


      dimension mx(iddo),my(iddo)


      dimension sv(ida),sv1(ida),sv2(ida),ior(ida)

      dimension ixb(ida),iyb(ida)

      dimension xc(ida),yc(ida),snr(ida),apix(ida),cra(ida),cann(ida),
     ?cwid(ida),marca(ida),marcax(ida),marcay(ida),bcg(ida),bcgs(ida),
     ?cf(ida),nbcg(ida)



c
c     Initial values
c


      pi=3.141592653589793D0
      grarad=pi/180.d0
      radgra=180.d0/pi


      dp=2.d0

      iali=5

      gbin=1.d0

      gwi=5.d0

      widi=2.d0

      index=-1

      nterms=2

      see=nn

c
c     Applying a median filter to set the local sky background
c     through all the FOV. This "equalyzes' the counts to better
c     sort the cells in crescent flux order. Only cells above a
c     local threshold are sorted. The local threshold equals the
c     local sky background plus 1 sigma, sigma being the
c     local standard deviation about the local sky background
c     average. In this way we induce the selection of cells closer
c     to the center of true objects and discard the ones at
c     the borders of the objects, improving the quality of the
c     detections and speeding up the computations. The pixels
c     associated to the sky background in the median filter are
c     taken by following a quartile statistics procedure through
c     the subroutine "quarte".
c
c     For the median filter computations, the FOV is divided in
c     nxn parts, n being a fraction of the FOV. Here we set a
c     default percentage fraction of 2.5%=0.025 of the FOV, so
c     that it can be sensitive to fast variations of the sky
c     background along the FOV but at the same time avoid problems
c     with bright sources (large objects in the FOV).
c


      write (*,*)
      write (*,*)
      write (*,*) 'PRAIA BOIA method of identifying objects in the FOV. 
     ?'
      write (*,*)
      write (*,*)
      write (*,*)
      write (*,*) 'Applying a median filter in the FOV for local'
      write (*,*) 'sky background estimation ...'
      write (*,*)
      write (*,*)



      fotor=1.0d0

      ndois=2

      facell=0.025d0

      nnx=nx*facell
      nny=ny*facell


      nax=mod(nx,nnx)
      nay=mod(ny,nny)

      nbx=mod(nax,ndois)
      nby=mod(nay,ndois)

      nborx=nax/2.d0+0.1
      nbory=nay/2.d0+0.1


      if (nbx.eq.0) then

      nx1=1+nborx
      nx2=nx-nborx

      else

      nx1=1+nborx+1
      nx2=nx-nborx

      endif


      if (nby.eq.0) then

      ny1=1+nbory
      ny2=ny-nbory

      else

      ny1=1+nbory+1
      ny2=ny-nbory

      endif


c
c     Operates over the middle of the FOV
c

      do    kk=ny1,ny2,nny

      do 05 jj=nx1,nx2,nnx


      n=0

      do k=kk,kk+nny-1
      do j=jj,jj+nnx-1
      if (imagem(j,k).eq.0) then
      n=n+1
      ior(n)=n
      sv1(n)=pixmat(j,k)
      endif
      enddo
      enddo


      if (n.eq.0) go to 05

      call dordem (ida,n,ior,sv1)

      dn=dble(n)

      nq=idnint(dn*(1.d0-qua))

      call quarte (iddo,n,ior,sv1,nq,n1,n2)

      n=0

      do i=n1,n2
      n=n+1
      sv(n)=sv1(ior(i))
      enddo

      call avevar(sv,n,ave,var)

      var=dsqrt(var)

      do k=kk,kk+nny-1
      do j=jj,jj+nnx-1
      if (imagem(j,k).eq.0) then
      matpix(j,k)=ave
      matpit(j,k)=ave+fotor*var
      endif
      enddo
      enddo


 05   continue
      enddo


c
c     Operates over the FOV lateral borders
c
c     Here it simply copies the nearest results
c     from the lateral borders of the middle of the FOV
c

      IF (nax.ne.0) THEN

      do k=ny1,ny2

      do j=1,nx1-1

      if (imagem(j,k).eq.0) then

      matpix(j,k)=matpix(nx1,k)
      matpit(j,k)=matpit(nx1,k)

      endif

      enddo


      do j=nx2+1,nx

      if (imagem(j,k).eq.0) then

      matpix(j,k)=matpix(nx2,k)
      matpit(j,k)=matpit(nx2,k)

      endif

      enddo

      enddo

      ENDIF


      IF (nay.ne.0) THEN


      do j=nx1,nx2

      do k=1,ny1-1

      if (imagem(j,k).eq.0) then

      matpix(j,k)=matpix(j,ny1)
      matpit(j,k)=matpit(j,ny1)

      endif

      enddo


      do k=ny2+1,ny

      if (imagem(j,k).eq.0) then

      matpix(j,k)=matpix(j,ny2)
      matpit(j,k)=matpit(j,ny2)

      endif

      enddo

      enddo

      ENDIF


c
c     Operates over the FOV corners
c
c     Here it simply copies the nearest results
c     from the corners of the middle of the FOV
c


      do k=1,ny1-1

      do j=1,nx1-1

      if (imagem(j,k).eq.0) then

      matpix(j,k)=matpix(nx1,ny1)
      matpit(j,k)=matpit(nx1,ny1)

      endif

      enddo

      do j=nx2+1,nx

      if (imagem(j,k).eq.0) then

      matpix(j,k)=matpix(nx2,ny1)
      matpit(j,k)=matpit(nx2,ny1)

      endif

      enddo


      enddo


      do k=ny2+1,ny

      do j=1,nx1-1

      if (imagem(j,k).eq.0) then

      matpix(j,k)=matpix(nx1,ny2)
      matpit(j,k)=matpit(nx1,ny2)

      endif

      enddo

      do j=nx2+1,nx

      if (imagem(j,k).eq.0) then

      matpix(j,k)=matpix(nx2,ny2)
      matpit(j,k)=matpit(nx2,ny2)

      endif

      enddo


      enddo



c     write (*,*) 
c     write (*,*) 'nnx,nny     = ',nnx,nny
c     write (*,*) 'nax,nay     = ',nax,nay
c     write (*,*) 'nbx,nby     = ',nbx,nby
c     write (*,*) 'nborx,nbory = ',nborx,nbory
c     write (*,*) 'nx1,nx2     = ',nx1,nx2
c     write (*,*) 'ny1,ny2     = ',ny1,ny2
c     write (*,*) 'idimx,idimy = ',idimx,idimy
c     write (*,*) 'ida,iddo    = ',ida,iddo
c     write (*,*) 'qua         = ',qua
c     write (*,*)
c     write (*,*)
c
c
c
c     write (803) ((matpix(j,i),j=1,nx),i=1,ny)
c     write (804) ((((matpit(j,i)-matpix(j,i))/fotor),j=1,nx),i=1,ny)
c     write (805) ((matpit(j,i),j=1,nx),i=1,ny)
c
c
c  mv fort.803 ave.arr
c  mv fort.804 var.arr
c  mv fort.805 var2.arr
c  ds9 -array ave.arr[xdim=2048,ydim=2048,zdim=1,bitpix=-32,skip=4,arch=littleendian]
c  ds9 -array var2.arr[xdim=2048,ydim=2048,zdim=1,bitpix=-32,skip=8,arch=littleendian]
c  ds9 -array var.arr[xdim=2048,ydim=2048,zdim=1,bitpix=-64,skip=4,arch=littleendian]



c
c     Divides image in cells of nn x nn pixels and picks
c     up candidate object detections.
c
c     (Here, we fixed the cell sizes in 3x3 pixels).
c


      write (*,*)
      write (*,*)
      write (*,*) 'Scanning image for object detections ...'
      write (*,*)
      write (*,*)


      nnx=nn
      nny=nn


      nax=mod(nx,nnx)
      nay=mod(ny,nny)

      nbx=mod(nax,ndois)
      nby=mod(nay,ndois)

      nborx=nax/2.d0+0.1
      nbory=nay/2.d0+0.1


      if (nbx.eq.0) then

      nx1=1+nborx
      nx2=nx-nborx

      else

      nx1=1+nborx+1
      nx2=nx-nborx

      endif


      if (nby.eq.0) then

      ny1=1+nbory
      ny2=ny-nbory

      else

      ny1=1+nbory+1
      ny2=ny-nbory

      endif


c     write (*,*) 
c     write (*,*) 'nnx,nny     = ',nnx,nny
c     write (*,*) 'nax,nay     = ',nax,nay
c     write (*,*) 'nbx,nby     = ',nbx,nby
c     write (*,*) 'nborx,nbory = ',nborx,nbory
c     write (*,*) 'nx1,nx2     = ',nx1,nx2
c     write (*,*) 'ny1,ny2     = ',ny1,ny2
c     write (*,*) 
c     write (*,*) 



      m=0


      do    kk=ny1,ny2,nny

      do 10 jj=nx1,nx2,nnx


      do k=kk,kk+nny-1
      do j=jj,jj+nnx-1

      if (imagem(j,k).ne.0.or.pixmat(j,k).lt.matpit(j,k)) goto 10

      enddo
      enddo


      m=m+1
      mx(m)=j+nnx/2.d0
      my(m)=k+nny/2.d0


 10   continue

      enddo



c
c     Warns that the maximum allowed number of candidate detections
c     was surpassed.
c


      if (m.gt.iddo) then

      write (*,*)
      write (*,*)
      write (*,*)
      write (*,*) 'Maximum allowed number of object detections was'
      write (*,*) 'surpassed. The probability that the BOIA procedure'
      write (*,*) 'fails in detecting the FOV objects is very high ...'
      write (*,*)
      write (*,*)
      write (*,*)

      endif



c     do kk=1,m
c     k=ior(kk)
c     write (800,*) kk,sv1(k),mx(k),my(k)
c     enddo


      mcan1=1
      mcan2=m


c     write (*,*)
c     write (*,*)
c     write (*,*) 'mcan1 = ',mcan1 
c     write (*,*) 'mcan2 = ',mcan2
c     write (*,*) 'ida,iddo,nn = ',ida,iddo,nn
c     write (*,*)
c     stop


c
c     Identification of objects by the PRAIA Object Ultimate Detection
c     (BOIA) method.
c
c
c     The objects are identified from brigher to lower preliminary count
c     candidates.
c
c     The search stops when the last matrix cell is evaluated.
c
c     Valid candidates are objects which are not artifacts of the image,
c     such as groups of pixels around saturated objects or detections at
c     the FOV borders.
c
c     Some extra care is taken to further filter out false-positives:
c
c     - elimination of multiple detections of the same object or part of it;
c
c     - elimination of false positive detections nearby the limits of
c       the FOV;
c
c     - elimination of cosmic rays;
c
c     - elimination of false detections due to vertical and horizontal flux
c       leak from saturated objects or from strong difraction spikes.
c
c
c     The identification of each individual candidate object, and the
c     determination of its main (optimized) parameters (center, flux,
c     S/N ratio, sky background and sky background sigma, among other
c     auxiliary quantities) is basically carried out by the subroutine
c     centob.
c
c


      write (*,*)
      write (*,*)
      write (*,*) 'Scanning concluded. Selecting detections.'
      write (*,*)
      write (*,*)



c
c     Main object search loop
c

      m=0

      do 90 i=mcan1,mcan2


c     write (*,*) i,mx(i),my(i)


      if (mx(i).gt.nx) go to 90
      if (my(i).gt.ny) go to 90

      xg=mx(i)
      yg=my(i)

      mark=0

c
c     Determines astrometry/photometry optimal parameters according
c     to the best possible S/N.
c

      gann=see

      call centob (idimx,idimy,ida,sv,ixb,iyb,ior,pixmat,nx,ny,xg,yg,
     ?gra,gbin,gann,gwi,ag,nbg,snrat,gain,mark,sv1,sv2,fluxo,sbg,sbg2,
     ?pi,see,widi,qua,gramal)


      if (mark.eq.1) go to 90



c
c     Excludes candidate object pixels from future search, regardless if
c     the candidate object is true or a false detection.
c


c     ixr=xg
c     iyr=yg
c
c     iraio=gra
c
c     jx1=ixr-iraio
c     jx2=ixr+iraio
c     jy1=iyr-iraio
c     jy2=iyr+iraio
c
c     if (jx1.lt.1)  jx1=1
c     if (jx2.gt.nx) jx2=nx
c     if (jy1.lt.1)  jy1=1
c     if (jy2.gt.ny) jy2=ny
c
c
c     do    k=jy1,jy2
c     do 15 j=jx1,jx2
c 
c
c     if (imagem(j,k).lt.0) go to 15
c
c     call circul (gra,xg,yg,j,k,ichave)
c
c     if (ichave.gt.0) imagem(j,k)=-2
c 
c15   continue
c     enddo


c
c     Stores astrometry/photometry parameters of candidate object
c     for further check to confirm if it is really a true object.
c

c     if (fluxo.lt.2000) go to 90

      m=m+1

      xc(m)=xg
      yc(m)=yg
      cra(m)=gra
      cann(m)=gann
      snr(m)=snrat
      apix(m)=ag
      cf(m)=fluxo
      nbcg(m)=nbg
      bcg(m)=sbg
      bcgs(m)=sbg2
      marca(m)=0


c     write (*,*) m,xc(m),yc(m),cf(m)

c
c     Stops the search if the maximum allowed number of object
c     detections was reached. 
c


      if ((m+1).gt.ida) then

      write (*,*)
      write (*,*)
      write (*,*)
      write (*,*) 'Maximum allowed number of object detections found. St
     ?opping search in N = ',m
      write (*,*)

      go to 100

      endif



 90   continue


c
c     First step of identification of objects terminated.
c


 100  continue



      write (*,*)
      write (*,*)
      write (*,*) 'Selection terminated.'
      write (*,*)
      write (*,*)




c
c     Restores marked pixels from canditate object search
c


c     do i=1,ny
c     do j=1,nx
c     if (imagem(j,i).eq.-2) imagem(j,i)=0
c     enddo
c     enddo




c
c     Eliminates multiple detections of the same object or
c     part of it.
c
c     Here, multiple detections are identified when their apertures
c     intercept each other.
c
c     The detection with the largest aperture is preserved.
c

      write (*,*)
      write (*,*)
      write (*,*) 'Eliminating multiple false detections of the'
      write (*,*) 'same object or part of it when the detection'
      write (*,*) 'center falls inside another detection aperture ...'
      write (*,*)
      write (*,*)


      do     k=1,m-1
      do 110 j=k+1,m


      d=dsqrt((xc(j)-xc(k))**2+(yc(j)-yc(k))**2)

      if (d.gt.cra(k).and.d.gt.cra(j)) go to 110

c     if (d.gt.(cra(k)+cra(j))) go to 110


c     if (snr(j).lt.snr(k)) then
      if (cra(j).lt.cra(k)) then
      marca(j)=1
      else
      marca(k)=1
      endif

 110  continue
      enddo

      n=0
      
      do 115 k=1,m

         
      if (marca(k).ne.0) go to 115

      n=n+1

      marca(n)=0

      xc(n)=xc(k)
      yc(n)=yc(k)
      cra(n)=cra(k)
      cann(n)=cann(k)
      snr(n)=snr(k)

      apix(n)=apix(k)
      cf(n)=cf(k)
      nbcg(n)=nbcg(k)
      bcg(n)=bcg(k)
      bcgs(n)=bcgs(k)

c     write (*,*) xc(n),yc(n),cra(n),cann(n),apix(n),snr(n)


 115  continue


      m=n



c
c     Elimination of false detections usually due to vertical
c     and horizontal flux leak from saturated objects or from
c     their strong difraction spikes.
c
c     The vertical/horizontal alignment is characterized
c     if "iali" (here, 5 or more) detections have centers aligned
c     with the center of usually saturated/bright detections.
c
c     When this happens, the central saturated/bright detection
c     with the largest aperture is preserved and the other aligned
c     fainter detections are all eliminated.
c
c     The objects to be checked are those brighter than the first
c     object to present a strong gradient in flux, above 50%.
c     Thus, depending on the field, some or no sources may undergo
c     the search, reflecting the presence or not of saturated
c     or difraction-spiked objects in the FOV.
c
c     The object's flux gradient is computed as a percentage (0%-100%
c     or above; negative values are possible) with respect to the
c     object flux of the difference between the mean flux of the
c     immediate brighter and fainter objects and the object flux
c     itself:
c
c
c                      mf - cf(i)
c      gra(%) = 100 * ------------
c                        cf(i)
c
c                  cf(i+1) + cf(i-1)
c      where  mf = --------------------
c                          2
c
c
c      with cf(i) = the object's flux
c      
c     



      write (*,*)
      write (*,*)
      write (*,*) 'Eliminating false positive detections due to'
      write (*,*) 'vertical and horizontal flux leak, difraction spikes'
      write (*,*) 'and difuse light from bright/saturated objects ...'
      write (*,*)
      write (*,*)


c
c     If less than "iali+1" detections (here, iali+1=6) are present,
c     skip procedure, as any alignement is considered fortuitous in this
c     case.
c


      if (m.le.iali+1) goto 146


c
c     Detects outstanding bright/saturated sources, the only objects
c     which can provoque leaks or diffraction spikes in the FOV
c

      do i=1,m
      ior(i)=i
      enddo


      call dordem (ida,m,ior,cf)


      do ii=m-1,2,-1

      i0=ior(ii)
      i1=ior(ii-1)
      i2=ior(ii+1)

      n=m-ii

      sv(n)=100.d0*((cf(i2)+cf(i1))/2.d0-cf(i0))/cf(i0)

      enddo



      kk=n/2

      do ii=kk,1,-1
 
      if (sv(ii).gt.50.d0) go to 117

      enddo
 
 117  n=m-ii+1


      if (n.gt.m) go to 146


c
c
c     Outsdanding bright detections found in the FOV.
c     Proceed with the investigation of spurious detections
c     around these bright detections due to saturation and
c     difraction spikes.
c
c
c     For each selected bright detection, searches for detection
c     alignments on the x and y directions. If, from a statistical
c     point of view, the alignement is unusual as compared to the
c     alignement of other FOV region with about the same number of
c     detections, then the detections within the alignement are
c     set as spurious. The aligned detections are sampled within
c     1 bright source aperture radius in x and y directions separately,
c     and the variance of their centers are tested with the F-test to
c     find if there is or not an alignement.
c
c 
c


c
c     Loop of bright detections
c

      qqua=0.5d0

      DO ii=m,n,-1

      i=ior(ii)
         
      
c
c     Vertical alignements
c

      nnb=0

      do j=1,m

      dx=dabs(xc(j)-xc(i))

      if (dx.le.cra(i)) then

         nnb=nnb+1
         ixb(nnb)=nnb
         sv(nnb)=cra(j)

      endif

      enddo


c
c     Sets the typical aperture size of detections
c     associated to the bright detection
c

      call dordem (ida,nnb,ixb,sv)

      nq=idnint(nnb*(1.d0-qqua))

      call quarte (ida,nnb,ixb,sv,nq,n1,n2)

      jj=0
      do j=n1,n2

         jj=jj+1
         k=ixb(j)
         sv1(jj)=sv(k)

      enddo

      call avevar (sv1,jj,cram,cras)



      
c
c     Sampling the detections associated to the
c     bright detection. Keeps the sampling with
c     the higher number of detections.
c

      kbin=(2.d0*cra(i))/cram
      kbin=kbin+1

      do jj=1,kbin
      sv(jj)=0.d0
      enddo
 

      
      do j=1,m
         
      dx=xc(j)-xc(i)

      if (dabs(dx).le.cra(i)) then

         nbin=(dx+cra(i))/cram
         nbin=nbin+1

         sv(nbin)=sv(nbin)+1.d0

      endif
         
      enddo
      

      nnb=0

      do j=1,kbin

         if (sv(j).gt.nnb) nnb=sv(j)

      enddo


      
      
c
c     Sampling the FOV for alignements
c     within the same size obtained for
c     the bright detection.
c

      kbin=nx/cram
      kbin=kbin+1

      do jj=1,kbin
      sv(jj)=0.d0
      enddo
 

      
      do j=1,m
         
         nbin=xc(j)/cram
         nbin=nbin+1

         sv(nbin)=sv(nbin)+1.d0

      enddo
      
c
c     Avoids contamination by the bright detections
c     of the FOV
c

      
      do jj=n,m
         k=ior(jj)
         nbin1=(xc(k)-cra(k))/cram
         nbin2=(xc(k)+cra(k))/cram
         nbin1=nbin1+1
         nbin2=nbin2+1
         do kk=nbin1,nbin2
         if (kk.ge.1) sv(kk)=-1.d0
         enddo
      enddo   

      
c
c     Frequency and standard deviation of the
c     alignements of the sampled FOV
c


      jj=0
      
      do j=1,kbin
         
         if(sv(j).ge.0.d0) then
            jj=jj+1
            sv(jj)=sv(j)
         endif
         
      enddo


      call avevar (sv,jj,aves,vars)

      vars=dsqrt(vars)

      
c
c     Does the aligned detections of the current bright
c     detection significantly deviate from the sampled alignements
c     of the FOV?
c
c     If so, we have spurious detections.
c

      dd=dabs((nnb-aves)/vars)

c     write (*,*)
c     write (*,*) ' V nnb,aves,vars,dd = ',nnb,aves,vars,dd
c     write (*,*)
      

      if (dd.gt.10.d0.and.nnb.gt.iali) then

         do j=1,m

            dx=dabs(xc(j)-xc(i))

            if (dx.le.cra(i)) marca(j)=1

         enddo

      endif




      
c
c     Horizontal alignements
c

      nnb=0

      do j=1,m

      dy=dabs(yc(j)-yc(i))

      if (dy.le.cra(i)) then

         nnb=nnb+1
         iyb(nnb)=nnb
         sv(nnb)=cra(j)

      endif

      enddo


c
c     Sets the typical aperture size of detections
c     associated to the bright detection
c

      call dordem (ida,nnb,iyb,sv)

      nq=idnint(nnb*(1.d0-qqua))

      call quarte (ida,nnb,iyb,sv,nq,n1,n2)

      jj=0
      do j=n1,n2

         jj=jj+1
         k=iyb(j)
         sv1(jj)=sv(k)

      enddo

      call avevar (sv1,jj,cram,cras)



      
c
c     Sampling the detections associated to the
c     bright detection. Keeps the sampling with
c     the higher number of detections.
c

      kbin=(2.d0*cra(i))/cram
      kbin=kbin+1

      do jj=1,kbin
      sv(jj)=0.d0
      enddo
 

      
      do j=1,m
         
      dy=yc(j)-yc(i)

      if (dabs(dy).le.cra(i)) then

         nbin=(dy+cra(i))/cram
         nbin=nbin+1

         sv(nbin)=sv(nbin)+1.d0

      endif
         
      enddo
      

      nnb=0

      do j=1,kbin

         if (sv(j).gt.nnb) nnb=sv(j)

      enddo


      
      
c
c     Sampling the FOV for alignements
c     within the same size obtained for
c     the bright detection.
c

      kbin=ny/cram
      kbin=kbin+1

      do jj=1,kbin
      sv(jj)=0.d0
      enddo
 

      
      do j=1,m
         
         nbin=yc(j)/cram
         nbin=nbin+1

         sv(nbin)=sv(nbin)+1.d0

      enddo
      
c
c     Avoids contamination by the bright detections
c     of the FOV
c

      
      do jj=n,m
         k=ior(jj)
         nbin1=(yc(k)-cra(k))/cram
         nbin2=(yc(k)+cra(k))/cram
         nbin1=nbin1+1
         nbin2=nbin2+1
         do kk=nbin1,nbin2
         if (kk.ge.1) sv(kk)=-1.d0
         enddo
      enddo   

      
c
c     Frequency and standard deviation of the
c     alignements of the sampled FOV
c


      jj=0
      
      do j=1,kbin
         
         if(sv(j).ge.0.d0) then
            jj=jj+1
            sv(jj)=sv(j)
         endif
         
      enddo


      call avevar (sv,jj,aves,vars)

      vars=dsqrt(vars)

      
c
c     Does the aligned detections of the current bright
c     detection significantly deviate from the sampled alignements
c     of the FOV?
c
c     If so, we have spurious detections.
c

      dd=dabs((nnb-aves)/vars)

c     write (*,*)
c     write (*,*) ' H nnb,aves,vars,dd = ',nnb,aves,vars,dd
c     write (*,*)
      

      if (dd.gt.10.d0.and.nnb.gt.iali) then

         do j=1,m

            dy=dabs(yc(j)-yc(i))

            if (dy.le.cra(i)) marca(j)=1

         enddo

      endif



      
      marca(i)=0
         

      ENDDO

      
         

c
c     Eliminates spurious detections around the
c     outstanding bright/saturated sources.
c
c     These spurious detections are searched around a radius equal
c     to 2 times the sky background radius of the bright source.
c


c     if (dx**2+dy**2.lt.cann(i)**2) then
c
c     if (cra(j).eq.cann(j)) marca(j)=1
c
c     endif


c     if (dx**2+dy**2.lt.4.d0*cann(i)**2) marca(j)=1




c
c     Stores not eliminated detections
c


      n=0

      do 145 k=1,m

c     write (54,55) xc(k),yc(k),cra(k)


      if (marca(k).eq.1) go to 145

      n=n+1

      marca(n)=0

      xc(n)=xc(k)
      yc(n)=yc(k)
      cra(n)=cra(k)
      cann(n)=cann(k)
      snr(n)=snr(k)

      apix(n)=apix(k)
      cf(n)=cf(k)
      nbcg(n)=nbcg(k)
      bcg(n)=bcg(k)
      bcgs(n)=bcgs(k)


c     write (55,55) xc(n),yc(n),cra(n)
c55   format('circle(',2(f8.2,','),f8.2,')')
c
c     write (800,55) xc(n),yc(n),cra(n)
c     write (801,55) xc(n),yc(n),cann(n)
c     write (802,55) xc(n),yc(n),cra(n)+cann(n)
c55   format('circle(',2(f8.2,','),f8.2,')')
c     write (*,*) xc(n),yc(n),cra(n),cann(n),apix(n),snr(n)



 145  continue

      m=n


 146  continue




c
c     Eliminates spurious faint detections characterized by
c     presenting the same radius for the optimized aperture
c     and the sky background ring.
c
c     These spurious detections may be present all around
c     the FOV.
c
c     When the radii are the same, a f-test is performed over
c     the pixel count distributions inside the aperture and the
c     sky background ring. If the test indicates that both
c     pixel distributions are statistically indistinguishable,
c     the detection is eliminated.
c


      write (*,*)
      write (*,*)
      write (*,*) 'Eliminating false positive faint detections'
      write (*,*) 'characterized by presenting the same radius'
      write (*,*) 'for the optimized aperture and the sky'
      write (*,*) 'background ring ...'
      write (*,*)
      write (*,*)




      n=0

      do 147 k=1,m

      if (cra(k).eq.cann(k)) then


c
c
c     aperture pixels: parameters of distribution
c
c
c     number             = n1
c     average            = ave
c     standard deviation = var
c
c

      n1=apix(k)

      call pixap (idimx,idimy,ida,pixmat,nx,ny,xc(k),yc(k),cra(k),sv,n1)


      call avevar(sv,n1,ave,var)

      var=dsqrt(var)



c
c
c     ring pixels: parameters of distribution
c
c     number             = nbcg
c     average            = bcg
c     standard deviation = bcgs
c
c



c
c     Tests the sigma of the aperture pixels against the
c     sigma of the ring pixels.
c
c
c     Using the F-test, checks if the two sigmas are
c     statistically different. If they are statistically
c     equal, eliminates the detection.
c
c
c     The threshold P of probability rejection
c     corresponds to a X sigma factor rejection
c     criterion such that:
c
c
c     P = 1 - 0.382924922548026   ====>  X = 0.5
c
c     P = 1 - 0.682689492137086   ====>  X = 1.0
c
c     P = 1 - 0.866385597462284   ====>  X = 1.5
c
c     P = 1 - 0.954499736103642   ====>  X = 2.0
c
c     P = 1 - 0.997300203936740   ====>  X = 3.0
c
c     P = 1 - 0.999936657516334   ====>  X = 4.0
c
c     etc
c


      call ftesta (n1,nbcg(k),var,bcgs(k),f,prob)



c     if (prob.ge.(1.d0-0.382924922548026d0)) go to 147


      if (prob.ge.5.d-2) go to 147


c     if (prob.ge.(1.d0-0.682689492137086d0)) go to 147
c     if (prob.ge.10.d-2) go to 147
c     if (prob.ge.5.d-2) go to 147
c     if (prob.ge.(1.d0-0.997300203936740d0))  go to 147
c     if (prob.ge.(1.d0-0.999936657516334d0)) go to 147



      endif 



      n=n+1

      xc(n)=xc(k)
      yc(n)=yc(k)
      cra(n)=cra(k)
      cann(n)=cann(k)
      snr(n)=snr(k)

      apix(n)=apix(k)
      cf(n)=cf(k)
      nbcg(n)=nbcg(k)
      bcg(n)=bcg(k)
      bcgs(n)=bcgs(k)


c     write (800,55) xc(n),yc(n),cra(n)
c     write (801,55) xc(n),yc(n),cann(n)
c     write (802,55) xc(n),yc(n),cra(n)+cann(n)
c55   format('circle(',2(f8.2,','),f8.2,')')
c     write (*,*) xc(n),yc(n),cra(n),cann(n),apix(n),snr(n)



 147  continue

      m=n





c
c     Elininates false positive detections nearby the
c     limits of the FOV;
c

      write (*,*)
      write (*,*)
      write (*,*) 'Eliminating false positive detections nearby the'
      write (*,*) 'limits of the FOV, and true detections too close'
      write (*,*) 'to the FOV borders ...'
      write (*,*)
      write (*,*)




      do 121 k=1,m

      if (xc(k)-cra(k).lt.0.5d0) marca(k)=1
      if (yc(k)-cra(k).lt.0.5d0) marca(k)=1

      if (xc(k)+cra(k).gt.nx+0.5d0) marca(k)=1
      if (yc(k)+cra(k).gt.ny+0.5d0) marca(k)=1


 121  continue

      n=0
      

      do 125 k=1,m

      if (marca(k).ne.0) go to 125

      n=n+1

      marca(n)=0

      xc(n)=xc(k)
      yc(n)=yc(k)
      cra(n)=cra(k)
      cann(n)=cann(k)
      snr(n)=snr(k)

      apix(n)=apix(k)
      cf(n)=cf(k)
      nbcg(n)=nbcg(k)
      bcg(n)=bcg(k)
      bcgs(n)=bcgs(k)

c     write (*,*) xc(n),yc(n),cra(n),cann(n),apix(n),snr(n)

 125  continue

      m=n




c
c     Eliminate cosmic rays and hot pixels
c


      write (*,*)
      write (*,*)
      write (*,*) 'Eliminating false positive detections that are'
      write (*,*) 'actually cosmic rays or hot pixels ...'
      write (*,*)
      write (*,*)



c
c     If less than "iali+1" detections (here, iali+1=6) are present,
c     skip procedure, as any cosmic ray statistics makes no sense
c     in this case.
c


      if (m.le.iali+1) goto 156



c
c     Order candidates as a function of the
c     contrast defined as:
c
c     flux / area^2
c
c
c     Ordered in crescent contrast order, all true objects follow a
c     smooth continuos growth curve. All cosmic rays and hot pixel
c     false candidate points appear at the end of this curve after
c     an abrupt contrast growth.
c
c     A F-test comparing n with the previous n-1 contrast points
c     indicate if the variance significantly changed from a statistical
c     point of view. If so, the nth point and the remaining ones are
c     set as cosmic rays. 
c
c

      do k=1,m

      ior(k)=k

      sv1(k)=cf(k)/apix(k)**2

      ixb(k)=k

      sv(k)=cra(k)

      enddo


      call dordem (ida,m,ior,sv1)

      call dordem (ida,m,ixb,sv)


      do i=1,m

      sv2(i)=sv1(ior(i))

      enddo


c     do i=m/2,m
c
c     call avevar (sv2,i-1,ave,var)
c
c     var=dsqrt(var)
c
c     dif=dabs((sv2(i)-ave)/var)
c
c     write (2002,*) i,dif,sv2(i),ave,var
c
c     enddo


      jj=m+1

      do i=m/2,m


      call avevar (sv2,i-1,ave1,var1)
      call avevar (sv2,i,ave2,var2)

      var1=dsqrt(var1)
      var2=dsqrt(var2)

      call ftesta (i-1,i,var1,var2,f,prob)


      if (prob.lt.5.d-2) then
 
      jj=i
 
      go to 152 
 
      endif
 
 
      enddo


c
c     Marks the cosmic rays
c

 152  continue


      do ii=jj,m

      i=ior(ii)

      
c
c     Is the aperture of this cosmic ray candidate
c     really small? 
c

      do mm=1,m/3
      k=ixb(mm)
      if (i.eq.k) marca(i)=1
      enddo


      enddo



c
c     Eliminates the cosmic rays / hot pixels
c


      n=0

      do 155 k=1,m


      if (marca(k).ne.0) go to 155



c
c     Excludes pixels associated to cosmic ray candidates
c     from the pixel matrix to avoid allowing them to enter in
c     future computations, like on improving the center with
c     the PRAIA PGC method, etc.
c
c
c     if (marca(k).ne.0) then
c
c     jx1=xc(k)-cra(k)
c     jx2=xc(k)+cra(k)
c     jy1=yc(k)-cra(k)
c     jy2=yc(k)+cra(k)
c
c     if (jx1.lt.1)  jx1=1
c     if (jx2.gt.nx) jx2=nx
c     if (jy1.lt.1)  jy1=1
c     if (jy2.gt.ny) jy2=ny
c
c
c     do i=jy1,jy2
c     do j=jx1,jx2
c
c     call circul (cra(k),xc(k),yc(k),j,i,ichave)
c
c     if (ichave.gt.0) then
c     pixmat(j,i)=-100.
c     imagem(j,i)=-2
c     endif
c 
c
c     enddo
c     enddo
c
c     go to 155
c
c     endif



      n=n+1

      marca(n)=0

      xc(n)=xc(k)
      yc(n)=yc(k)
      cra(n)=cra(k)
      cann(n)=cann(k)
      snr(n)=snr(k)

      apix(n)=apix(k)
      cf(n)=cf(k)
      nbcg(n)=nbcg(k)
      bcg(n)=bcg(k)
      bcgs(n)=bcgs(k)


c     xlado=cra(n)
c     write (33,55) xc(n),yc(n),xlado
c55   format('circle(',2(f8.2,','),f8.2,')')
c
c     write (7001,*) xc(n),yc(n),cra(n),cann(n),apix(n),snr(n)
c     write (*,*) xc(n),yc(n),cra(n),cann(n),apix(n),snr(n)


 155  continue

      m=n


 156  continue



c     do k=1,m
c
c     ior(k)=k
c
c     sv1(k)=cra(k)
c
c     enddo
c
c
c     call dordem (ida,m,ior,sv1)
c
c     n=0
c
c     do k=m,1,-1
c
c     kk=ior(k)
c
c     n=n+1
c
c     write (2005,*) n,sv1(kk),xc(kk),yc(kk)
c
c     enddo



c
c     These are the final identified objects
c     by the PRAIA BOIA method.
c

      write (*,*)
      write (*,*)
      write (*,*) 'PRAIA BOIA: object identification terminated.'
      write (*,*)
      write (*,*)



      return

      end


c
c
c
c     Subroutine centob
c
c
c     Purpose
c
c
c     Finds the best (x,y) center, circular aperture radius,
c     circular annulus radius and width for a given object,
c     based on its the best S/N ratio.
c
c
c     Main variables
c
c
c     idi,idim,ida - vector dimensions
c     sv,ior       - auxiliary variables
c
c     sv1,sv2      - pixel populations for statistic comparisons
c
c     nx,ny      - matrix dimensions
c     xg,yg      - input (x,y) center of object
c
c     gramal     - maximum aperture radius size limited by the dimension
c                  of variables (ida)
c
c     ksky       - S/N ratio formula option: (1) classic; (2) pixel-based
c     gra        - output circular aperture radius for optimal S/N ratio
c     bin        - aperture radius bin in best aperture radius determination
c     gann       - output circular annulus radius for optimal S/N ratio
c     ggwi       - output circular annulus width for optimal S/N ratio
c     gwi        - input maximum allowed annulus width
c     sper       - sky background average (quartile statistics)
c     sper       - sky background standard deviation (quartile statistics)
c     ag         - effective pixel area within circular aperture
c     nbg        - effective number of pixels within circular annulus
c     snrat      - optimal derived S/N ratio
c
c     gain       - CCD gain
c
c     mark       - astrometric error flag:
c                  0 = (x,y) was computed/updated successfully
c                  1 = (x,y) could not be computed/updated; in this case,
c                      the original (x,y) input is preserved and passed
c                      away
c
c
c
c     Auxiliary subroutines and functions
c
c
c     skycic
c     flux
c     pixring
c     circol
c     ftest
c     ttest
c     tutest
c
c     snrbg
c
c
c     In this version, the ring width is fixed and furnished by the
c     user for faster processing.
c
c
c
c     Last update:   M. Assafin - 01/Mar/2022
c
c



      subroutine centob (idimx,idimy,ida,sv,ixb,iyb,ior,pixmat,nx,ny,xg,
     ?yg,gra,bin,gann,gwi,ag,nbg,snrat,gain,mark,sv1,sv2,fluxo,sbg,sbg2,
     ?pi,see,widi,qua,gramal)


      implicit real*8 (A-H,O-Z)


      real*4 pixmat(idimx,idimy)

      dimension sv(ida),ior(ida),ixb(ida),iyb(ida)

      dimension sv1(ida),sv2(ida)


      snrbg(fobj,gain,apixel,nbg,sbg)=dsqrt(fobj/(1.d0/gain+(apixel*
     ?sbg**2)*(1.d0+1.d0/nbg)/fobj))



c
c     Start
c


c     pi=0.3141592653589793d1

c     zero=0.d0

c     widi=2.d0


      ipass=0

      mark=0

      ierro=0

c
c    Under the BOIA method (Browsing Objects: Identification and
c    Analysis) sets a provisional maximum circular aperture size, sky
c    background ring radius and width for the detection.
c


      gann=see


      call sbcirc (idimx,idimy,ida,pixmat,nx,ny,xg,yg,gann,widi,qua,ior,
     ?sv1,sv2,ierro)


      if (ierro.eq.1) then

      ierro=1
      nbg=0
      ag=0.d0
      sbg=0.d0
      sbg2=0.d0
      mark=1

      return

      endif


      gra=gann-widi

      if (gra.gt.gramal) gra=gramal

c
c     Refines the initial (x,y) center from the provisional maximum
c     circular aperture found. For that, uses the new Photogravity
c     Center Method (PGC).
c


      jx1=xg-gra 
      jx2=xg+gra 
      jy1=yg-gra 
      jy2=yg+gra 

      if (jx1.lt.1) jx1=1
      if (jy1.lt.1) jy1=1

      if (jx2.gt.nx) jx2=nx
      if (jy2.gt.ny) jy2=ny


c     call subar (ida,idimx,idimy,sv,ior,ixb,iyb,pixmat,nx,ny,xg,yg,
c    ?gra,jx1,jx2,jy1,jy2,xgb,ygb,nnjj)

      call bar (ida,idimx,idimy,sv,ior,ixb,iyb,pixmat,nx,ny,xg,yg,gra,
     ?jx1,jx2,jy1,jy2,xgb,ygb,nnjj)


      if (xgb.lt.0.5d0 .or. xgb.gt.(nx+0.5d0) .or. ygb.lt.0.5d0 .or.
     ?ygb.gt.(ny+0.5d0)) then

      ierro=1
      nbg=0
      ag=0.d0
      sbg=0.d0
      sbg2=0.d0
      mark=1

      return

      endif



c
c     Under the BOIA method (Browsing Objects: Identification and
c     Analysis), resets the maximum circular aperture size, sky
c     background ring radius and width for the detection.



      gann=see


      call sbcirc (idimx,idimy,ida,pixmat,nx,ny,xgb,ygb,gann,widi,qua,
     ?ior,sv1,sv2,ierro)


      if (ierro.eq.1) then

      ierro=1
      nbg=0
      ag=0.d0
      sbg=0.d0
      sbg2=0.d0
      mark=1

      return

      endif


      gra=gann-widi

      if (gra.gt.gramal) gra=gramal


c
c     Updates the initial (x,y) center from the maximum circular
c     aperture found. For that, uses the new Photogravity Center
c     Method (PGC).
c



      jx1=xgb-gra
      jx2=xgb+gra
      jy1=ygb-gra
      jy2=ygb+gra


      if (jx1.lt.1) jx1=1
      if (jy1.lt.1) jy1=1

      if (jx2.gt.nx) jx2=nx
      if (jy2.gt.ny) jy2=ny


      xx=xgb
      yy=ygb

c     call subar (ida,idimx,idimy,sv,ior,ixb,iyb,pixmat,nx,ny,xx,yy,
c    ?gra,jx1,jx2,jy1,jy2,xgb,ygb,nnjj)

      call bar (ida,idimx,idimy,sv,ior,ixb,iyb,pixmat,nx,ny,xx,yy,gra,
     ?jx1,jx2,jy1,jy2,xgb,ygb,nnjj)


      if (xgb.lt.0.5d0 .or. xgb.gt.(nx+0.5d0) .or. ygb.lt.0.5d0 .or.
     ?ygb.gt.(ny+0.5d0)) then

      ierro=1
      nbg=0
      ag=0.d0
      sbg=0.d0
      sbg2=0.d0
      mark=1

      return

      endif



c
c     Under the BOIA method (Browsing Objects: Identification and
c     Analysis), finds the optimum aperture parameters for the detection.
c
c     The optimum aperture parameters are those for which we get the
c     highest S/N ratio. This enhances either astrometric or photometric
c     measurements.
c
c     The aperture radius is increased by a fixed pre-defined bin (usually
c     0.1 pixels).
c
c     The initial aperture center was already found in the previous step.
c
c     The sky background ring radius also was already determined in the
c     previous step. It equals the maximum aperture radius to be sampled
c     in the best aperture radius search.
c
c     The sky background width on the other hand is searched by bins of
c     1 pixel, from a width of 1 up to a pre-defined limit (usually 5 pixels).
c
c


c
c     Aperture radius setup
c


c     ipix1=1

c     ipix2=idnint(pi*gra**2)


      gap1=2.d0
      gap2=gra

      if (gap1.gt.gap2) gap2=gap1

      nbins=idnint((gap2-gap1)/bin)

      if (nbins.eq.0) then
      nbins=1
      else
      nbins=nbins+1
      endif

c
c     Sky background ring radius setup
c


c     igann1=idnint(gann)
c
cc    igann2=idnint(2.d0*gann)
c
c     igann2=igann1





      igann1=gap2
 
c     igann2=igann1
 
      igann2=2.d0*gap2-1.d0
 
      if (igann2.lt.igann1) igann2=igann1




c
c     Sky background ring width setup
c

      igwi1=1
      igwi2=idnint(gwi)


c
c     Starting the aperture, ring radius, ring width
c     and (x,y) center loops
c



 15   continue


      snrat=-1.d14


c
c     Sets the fluxes of the pixelized apertures to be sampled
c     in the search for the best S/N ratio.
c
c
c     sv1 - pixel distances to the center in crescent order from
c           the center
c
c     sv2 - pixel counts of pixels in crescent order from the center
c
c     sv  - accumulated pixel counts of pixels in crescent order
c           from the center
c


      call pixapc (idimx,idimy,ida,pixmat,nx,ny,xgb,ygb,gra,ior,sv1,sv2,
     ?n)

      soma=0.d0

      do i=1,n
      soma=soma+sv2(i)
      sv(i)=soma
      enddo




c
c     Loop of sky background ring width
c


c     do 60 m=igwi1,igwi2
      do 60 m=igwi2,igwi2


      ggwi=m


c     ggwi=gwi



c
c     Loop of sky background ring radius
c
c     (Notice that actually there is no real loop here) 
c


      do 50 k=igann1,igann2,m


      gann=k



      call skycic (idimx,idimy,ida,sv2,ior,pixmat,nx,ny,xgb,ygb,gann,
     ?ggwi,qua,sper,sper2,nbg)


      if (nbg.le.1) go to 50




c
c     Loop of aperture radius
c


      do 40 j=1,nbins


      raio=gap1+(j-1)*bin

c     jj=idnint(pi*raio**2)
c
c     a=dble(jj)


      do jj=1,n

      if (sv1(jj).gt.raio) go to 25

      enddo

      jj=n+1

 25   jj=jj-1

      a=dble(jj)




c
c     The aperture flux and area
c


      pc=sv(jj)-a*sper

      sn=snrbg(dabs(pc),gain,a,nbg,sper2)

      if (pc.lt.0.d0) sn=-sn


c     write (6001,*) raio,a


      if (sn.gt.snrat) then

      snrat=sn

      igann=k
      igap=j
c     igwi=m

      ag=a
      ng=nbg

      fluxo=pc
      sbg=sper
      sbg2=sper2


      xc=xgb
      yc=ygb

c     exg=-1.d0
c     eyg=-1.d0


      endif


c
c     Closing aperture radius, ring width and radius loops
c


 40   continue

 50   continue

 60   continue


c

      if (ipass.eq.1) go to 70


c
c     Updates the (x,y) center by using the pixels within
c     the better aperture aperture just found. For that,
c     uses the new Photogravity Center Method (PGC).
c


      groc=gap1+(igap-1)*bin

      jx1=xc-groc
      jx2=xc+groc
      jy1=yc-groc
      jy2=yc+groc


      if (jx1.lt.1) jx1=1
      if (jy1.lt.1) jy1=1

      if (jx2.gt.nx) jx2=nx
      if (jy2.gt.ny) jy2=ny


c     call subar (ida,idimx,idimy,sv,ior,ixb,iyb,pixmat,nx,ny,xc,yc,
c    ?groc,jx1,jx2,jy1,jy2,xgb,ygb,nnjj)


      xgb=0.d0
      ygb=0.d0
      count=0.d0


      do    i=jy1,jy2
      do 65 j=jx1,jx2

      if (pixmat(j,i).lt.0.d0) go to 65

      call circul (groc,xc,yc,j,i,ichave)

      if (ichave.lt.0) go to 65

      xgb=xgb+j*pixmat(j,i)
      ygb=ygb+i*pixmat(j,i)
      count=count+pixmat(j,i)
      
 65   continue
      enddo

      xgb=xgb/count
      ygb=ygb/count



      if (xgb.lt.0.5d0 .or. xgb.gt.(nx+0.5d0) .or. ygb.lt.0.5d0 .or.
     ?ygb.gt.(ny+0.5d0)) then

      ierro=1
      nbg=0
      ag=0.d0
      sbg=0.d0
      sbg2=0.d0
      mark=1

      return

      endif


      ipass=1


c
c     Back to the aperture radius, ring width and radius loops
c

      go to 15


c
c     Closes the loops
c


 70   continue


c
c     Best aperture radius
c


      gra=gap1+(igap-1)*bin


c
c     Best sky background radius
c

      gann=igann

      nbg=ng

c
c     Best provisional (xc,yc) center
c


      xg=xc
      yg=yc


c
c     (x,y) center with problems
c


      if (xg.lt.0.5d0) then
      nbg=0
      ag=0.d0
      fluxo=0.d0
      sbg=0.d0
      sbg2=0.d0
      mark=1
      return
      endif



      if (yg.lt.0.5d0) then
      nbg=0
      ag=0.d0
      fluxo=0.d0
      sbg=0.d0
      sbg2=0.d0
      mark=1
      return
      endif




      if (xg.gt.(nx+0.5d0)) then
      nbg=0
      ag=0.d0
      fluxo=0.d0
      sbg=0.d0
      sbg2=0.d0
      mark=1
      return
      endif



      if (yg.gt.(ny+0.5d0)) then
      nbg=0
      ag=0.d0
      fluxo=0.d0
      sbg=0.d0
      sbg2=0.d0
      mark=1
      return
      endif


      return
      end





c
c
c      Subroutine apemag
c 
c
c      Estimates the aperture radius of an object or target as a function
c      of its magnitude based on the relation between the BOIA aperture and
c      magnitude of known objects (Gaia stars, user catalogue stars, ephemeris
c      catalogue objects).
c
c
c      The estimation is based on the empirical relation:
c
c
c         aperture  = A + B.mag + C.mag^2 
c
c
c
c      where the A, B and C coefficients are fitted by L.S. by using objects
c      with known magnitudes and measured BOIA apertures.
c
c
c
c      The inner sky background radius is computed in the same way. It can be
c      seen from many fittings that they follow the relation:
c
c
c             Inner sky background radius = 2 x aperture
c
c
c      (but the relation is not used; the inner sky background radius is computed
c      from the empirical relation as mentioned before).
c
c
c
c      Outliers are eliminated from the fit in a one-by-obe basis until no (O-C)
c      exceeds 3 sigma.
c
c
c
c      Variables:
c
c
c      nest   - number of (x,y) measured objects
c
c      id     - index of (x,y) measured objects pointing to the reference
c               object magnitudes:
c                                 0 - not a reference object
c                                 1 - it is a reference object
c
c      dmag   - magnitude of reference object
c
c      ap     - BOIA aperture of (x,y) measured object
c
c      amag1  - smaller magnitude (brighter object) used in the fit
c
c      amag2  - greater magnitude (fainter object) used in the fit
c
c      gramal - maximum aperture radius size limited by the dimension
c                  of variables (ida)
c
c      itira  - (RA,Dec) eliminated reference catalogue object index
c
c
c
c      Last modification: M. Assafin 01/Mar/2022
c 
c 

      subroutine apemag (idiobs,icofsp,coefx,xest,yest,xp,u,v,w,id,dmag,
     ?ap,nest,amag1,amag2,gramal,itira)


      implicit real*8 (a-h,o-z)

      dimension coefx(icofsp),wsig(idiobs),xest(idiobs),yest(idiobs),
     ?xp(idiobs),u(idiobs),v(idiobs),w(idiobs),kktira(idiobs)

      dimension id(idiobs),dmag(idiobs),ap(idiobs),itira(idiobs)


c
c     Initial data
c

      yy=0.d0

      index=-1

      nterms=3

      do i=1,nest
      kktira(i)=0
      wsig(i)=1.d0
      enddo


c
c     Fills in the variables
c


 10   continue

      j=0

      do i=1,nest

      if (id(i).ne.0.and.kktira(i).eq.0.and.ap(i).lt.gramal) then

      if (itira(i).eq.0) then

      j=j+1

      xest(j)=dmag(id(i))

c     xp(j)=dlog10(ap(i))

c     xp(j)=ap(i)**2

      xp(j)=ap(i)


c     write (511,*) j,xest(j),ap(i)
c     write (611,*) j,xest(j),ap(i)

      endif

      endif

      enddo

      do i=1,icofsp
      coefx(i)=0.d0
      enddo


c
c     The fit
c

      call svdfit (xest,yest,xp,wsig,j,coefx,nterms,u,v,w,idiobs,
     ?icofsp,chi,idiobs,icofsp,index)


c
c     Elimination of outliers
c
c     One by one outliers are eliminated until no point
c     has (O-C) higher than 3 x sigma.
c

      xm=0.d0
      xs=0.d0

      res=-1.d14

      jj=0

      do i=1,nest

      IF (id(i).ne.0.and.kktira(i).eq.0.and.ap(i).lt.gramal) THEN

      if (itira(i).eq.0) then

      jj=jj+1

      x=fpol(icofsp,index,xest(jj),yy,coefx,nterms,i,j)

      xx=x-xp(jj)

      if (dabs(xx).gt.res) then
      res=dabs(xx)
      ii=i
      endif

      xm=xm+xx
      xs=xs+xx**2

      endif

      ENDIF

      enddo


      call desvio (jj,xm,xs)


      xs=3.0d0*xs

      if (res.gt.xs) then
      kktira(ii)=1
      go to 10
      endif


c
c     Sets the brightest and faintest magnitude limits
c     effectively used in the fit.
c

      amag1=+1.d14
      amag2=-1.d14


      do i=1,nest

      IF (id(i).ne.0.and.kktira(i).eq.0.and.ap(i).lt.gramal) THEN

      if (itira(i).eq.0) then

      if (dmag(id(i)).lt.amag1) amag1=dmag(id(i))

      if (dmag(id(i)).gt.amag2) amag2=dmag(id(i))


      endif

      ENDIF

      enddo


c
c     If needed, resets the faintest magnitude limit as that
c     for which the BOIA aperture x magnitude relation reached
c     a minimum.
c

      amag3=+1.d14

      jj=0

      do i=1,nest

      IF (id(i).ne.0.and.kktira(i).eq.0.and.ap(i).lt.gramal) THEN

      if (itira(i).eq.0) then

      jj=jj+1

      x=fpol(icofsp,index,xest(jj),yy,coefx,nterms,i,j)

      if (x.lt.amag3) then
      amag3=x
      xx=xest(jj)
      endif

      endif

      ENDIF

      enddo


      if (amag2.gt.xx) amag2=xx


      return
      end



c
c
c      Subroutine prexy
c
c
c      Given the center, aperture radius, sky background ring radius
c      and width, furnishes complete photometry information and second
c      moments of the aperture region, in preparation for (x,y) PSF
c      fittings.
c
c
c
c
c
c      Variables:
c
c
c      gain   - CCD gain for S/N ratio computations (input)
c
c      xc,yc  - center (input)
c
c      r      - aperture radius (input)
c
c      rann   - sky background ring radius (input)
c
c      gramal - Maximum allowed aperture size due to dimension limits on
c               variables (idiobs)
c
c      qua    - quartile statistics percentage of elimination of outliers
c               in sky background ring computations
c
c      fc     - sky background
c
c      fs     - sky background dispersion
c
c      pc     - flux
c
c      a      - area (pixels) encompassed by the aperture
c
c      snra   - S/N ratio
c
c      u20,u11,u02 - second moments
c
c      teta   - rotation angle of major axis wrt the x axis (radians)
c
c      siga   - equivalent sigma
c
c      exca   - excentricity
c
c      ix1,ix2,iy1,iy2 - retangular region limits for PSF fit
c
c
c
c
c      Last modification: M. Assafin 01/Mar/2022
c 
c 

      subroutine prexy (idimx,idimy,idiobs,icofsp,pixmat,imagem,sv,ior,
     ?gain,coefx,xc,yc,r,rann,qua,fc,fs,nbg,pc,a,snra,u20,u02,u11,seg1,
     ?seg2,teta,siga,exca,ix1,ix2,iy1,iy2,nx,ny,ng,modpsf,ierro,gramal)

      implicit real*8 (a-h,o-z)

      integer*2 imagem(idimx,idimy)

      real*4 pixmat(idimx,idimy)

      dimension coefx(icofsp),sv(idiobs),ior(idiobs),ng(5)

      snrbg(fobj,gain,apixel,nbg,sbg)=dsqrt(fobj/(1.d0/gain+(apixel*
     ?sbg**2)*(1.d0+1.d0/nbg)/fobj))


c
c     Initial data
c

      ierro=0

      gwi=2.d0

c     gwi=5.d0


c
c     Maximum allowed aperture size due to dimension limits on
c     variables (idiobs)
c

      if (r.gt.gramal) r=gramal

c


      ix1=xc-r
      ix2=xc+r
      iy1=yc-r
      iy2=yc+r

      if (ix1.lt.1) ix1=1
      if (iy1.lt.1) iy1=1

      if (ix2.gt.nx) ix2=nx
      if (iy2.gt.ny) iy2=ny


c
c     Determines the sky background average count by mid-quartile
c     statistics
c


      call skycic (idimx,idimy,idiobs,sv,ior,pixmat,nx,ny,xc,yc,rann,
     ?gwi,qua,sper,sper2,nbg)


      if (nbg.le.1) then
      ierro=1
      return
      endif


      fc=sper

      fs=sper2


c
c     Determines the photometric flux
c

      call flux (idimx,idimy,pixmat,nx,ny,xc,yc,r,soma,a)

      if (a.lt.ng(modpsf)) then
      ierro=1
      return
      endif


      pc=soma-a*sper


c
c     Determines the S/N
c

      snra=snrbg(dabs(pc),gain,a,nbg,sper2)

      if (pc.lt.0.d0) snr=-snr


c
c     From the second moments of its image, estimates the semi-axes,
c     the equivalent sigma of the object, the orientation angle theta
c     and excentricity
c


      call momeci (idimx,idimy,icofsp,coefx,pixmat,ix1,ix2,iy1,iy2,xc,
     ?yc,r,u20,u11,u02,seg1,seg2,siga,teta,exca)



      return
      end





c
c
c    Subroutine skycic
c
c
c    Computes sky background from circular rings with quartile
c    statatistics. The percentage of outlier ring pixels is pre-defined.
c
c
c     Variables
c
c     ior   - variable indicating the crescent order of pixel counts
c     sv    - the pixel sample not ordered in crescent order
c     n     - number of pixels efectivelly used in the sky background
c             computations
c     nq    - separation length in pixels between the quartile limits 
c     n1    - lower quartile limit
c     n2    - higher quartile limit
c     qua   - percentage of elimination of outliers
c     x,y   - center (pixels)
c    radius - radius of annulus
c    width  - width of annulus 
c
c
c
c
c    Last modification:   M. Assafin    01/Mar/2022
c
c

      subroutine skycic (idimx,idimy,ida,sv,ior,pixmat,nx,ny,x,y,anulus,
     ?width,qua,sper,sper2,n)

      implicit real*8 (a-h,o-z)


      real*4 pixmat(idimx,idimy)

      dimension sv(ida),ior(ida)



c
c     Retrieves the pixels of the circular ring after
c     quartile statistics elimination of outliers
c


      call pixring (idimx,idimy,ida,pixmat,nx,ny,x,y,anulus,width,qua,
     ?ior,sv,n,ierro)

      sper=0.d0

      if (ierro.ne.0.or.n.le.0) then
      sper2=-1.d0
      return
      endif


      sper2=0.d0

      do i=1,n
      sper=sper+sv(i)
      sper2=sper2+sv(i)**2
      enddo


      call desvio (n,sper,sper2)


      return
      end






c
c
c    Subroutine flux
c
c
c    Computes object flux by circular aperture photometry
c
c    (x,y)           = object center 
c    radius          = aperture radius 
c
c
c    fluxo           = flux in ADUs, flagged pixels excluded
c    area            = actual number of pixels used on flux computation,
c                      flagged pixels excluded
c
c
c
c    For flux computations, the pixel is only computed when totally or
c    partially within a circular area with radius:
c
c     radius=radius
c
c     ichave : positive -> pixel within (totally or partially) circular area
c              negative -> pixel not within circular area
c
c
c
c     Last update: M. Assafin 20/Sep/2021
c


      subroutine flux (idimx,idimy,pixmat,nx,ny,x,y,radius,fluxo,area)

      implicit real*8 (a-h,o-z)

      real*4 pixmat(idimx,idimy)


c

      area=0
      fluxo=0.d0

c
c     Defines the rectangular area of the
c     object that contains the circular
c     aperture area
c
c     1 pixel is added for each side
c     as a safe margin
c


      jx1=x-radius-1.d0
      jx2=x+radius+1.d0
      jy1=y-radius-1.d0
      jy2=y+radius+1.d0


      if (jx1.lt.1) jx1=1
      if (jy1.lt.1) jy1=1

      if (jx2.gt.nx) jx1=nx
      if (jy2.gt.ny) jy1=ny


c

      do 2 k=jy1,jy2

      do 1 j=jx1,jx2


      if (pixmat(j,k).lt.0.d0) go to 1


      call circul (radius,x,y,j,k,ichave)


      if (ichave.lt.0) go to 1

      area=area+1.d0

      fluxo=fluxo+pixmat(j,k)


  1   continue
  2   continue

c

      return
      end




c
c
c
c     Subroutine pixring
c
c
c     Purpose
c
c
c     Returns the pixel counts of pixels inside a given circular ring.
c
c
c     Returned pixels are filtered by quartile statistics.
c
c
c     ior   - variable indicating the crescent order of pixel counts
c     sva   - internal auxiliary variable
c     sv    - the pixel sample not ordered in crescent order
c     n     - number of output pixels
c     nq    - separation length in pixels between the quartile limits 
c     n1    - lower quartile limit
c     n2    - higher quartile limit
c     qua   - percentage of elimination of outliers
c     xg,yg - ring center (pixels)
c
c
c
c
c     Last update:   M. Assafin - 01/Mar/2022
c
c
c

      subroutine pixring (idimx,idimy,ida,pixmat,nx,ny,xg,yg,anulus,
     ?width,qua,ior,sv,n,ierro)

      implicit real*8 (A-H,O-Z)


      real*4 pixmat(idimx,idimy)

      dimension sva(ida),sv(ida),ior(ida)

c

      ierro=0

c
c     Defines the matrix search limits
c

      raiom=anulus+width

      jx1=xg-raiom
      jx2=xg+raiom
      jy1=yg-raiom
      jy2=yg+raiom


      if (jx1.lt.1) jx1=1
      if (jy1.lt.1) jy1=1

      if (jx2.gt.nx) jx2=nx
      if (jy2.gt.ny) jy2=ny


c

      n=0

      do 2 k=jy1,jy2

      do 1 j=jx1,jx2



      if (pixmat(j,k).lt.0.d0) go to 1

      call circul (anulus,xg,yg,j,k,ichave)
      if (ichave.gt.0) go to 1
 
      call circul (raiom,xg,yg,j,k,ichave)
      if (ichave.lt.0) go to 1


c     call circol (anulus,xg,yg,j,k,ichave)
c     if (ichave.gt.0) go to 1
c
c     call circol (raiom,xg,yg,j,k,ichave)
c     if (ichave.lt.0) go to 1


      n=n+1

      sva(n)=pixmat(j,k)

      ior(n)=n

  1   continue
  2   continue

c

      if (n.le.0) then
      ierro=1
      return
      endif


c
c     Quartile statistics to eliminate intruding fluxes from
c     nearby objects, difraction spikes, optical distortions,
c     hot/cold pixels, etc.
c
c     The outlier ring pixels (percentage given by variable qua)
c     are discarded in order to pin down the most representative
c     ring pixels for future robust mean and standard deviation
c     statistics and other average/standard deviation tests.
c
c


c
c     Orders the pixel sample in crescent count order
c


      call dordem (ida,n,ior,sva)


c
c     Sets the separation length nq between the quartile limits 
c


      dn=dble(n)

      nq=idnint(dn*(1.d0-qua))


c
c     Only performs quartile statistics for cutoff of outliers
c     for a number of pixels equivalent to the area of a circle
c     of radius equal to or larger than 2 pixels (area > 12.5 pixels).
c     That is, performs quartile statistics and pixel cutoff only for
c     rings with nq > 12.
c
c     Otherwise, select all pixels found within the ring.
c
c


c     IF (nq.lt.13) THEN

      IF (nq.lt.2) THEN


      do i=1,n
      sv(i)=sva(ior(i))
      enddo

      return


      ELSE



c
c     Automatically finds lower and higher quartile limits
c


c     call quarte (ida,n,ior,sva,nq,n1,n2)


c     n1=n*qua/2.d0
c
c     if (n1.eq.0) n1=1
c 
c     n2=n*(1.d0-qua/2.d0)


      n1=1
 
      n2=nq


      ENDIF



c
c     Quartile pixel cutoff
c

      n=0

      do i=n1,n2
      n=n+1
      sv(n)=sva(ior(i))
      enddo



      return
      end





c
c
c    Subroutine sbcirc
c
c
c    Under the BOIA method (Browsing Objects: Identification and
c    Analysis) finds the sky background ring radius around a
c    candidate object by using circular rings.
c
c
c     gann       - input/output radius of internal border of the
c                  sky background ring
c
c     widi       - radius step for sky background ring radius search
c                  
c     qua        - percentage of elimination of outliers in quartile
c                  statistics
c
c
c
c    Last modified:  M. Assafin  17/Dec/2021 
c
c

      subroutine sbcirc (idimx,idimy,ida,pixmat,nx,ny,xg,yg,gann,widi,
     ?qua,ior,sv1,sv2,ierro)

      implicit real*8 (A-H,O-Z)


      real*4 pixmat(idimx,idimy)

      dimension sv1(ida),sv2(ida),ior(ida)



c
c     Initial values
c

      ierro=0

c
c     Sets a provisional circular aperture size, sky background ring
c     radius and width for the object, for the varying aperture option
c



      call pixring (idimx,idimy,ida,pixmat,nx,ny,xg,yg,gann,widi,qua,
     ?ior,sv1,n1,ierro)


      if (ierro.eq.1) return

c

  1   gann=gann+widi


      call pixring (idimx,idimy,ida,pixmat,nx,ny,xg,yg,gann,widi,qua,
     ?ior,sv2,n2,ierro)

      if (ierro.eq.1) return


c
c     Do the 2 pixel ring distributions have the same mean from the
c     statistical point of view of the Student-t test? If so, the sky
c     background was reached.
c
c     The F-test is previously performed in order to check if the 2
c     distributions have or not equal variances, and depending on
c     the two possibilities the correct Student-t test is applied
c     in accord to the Numerical Recipies (Press et al., 1982). 
c


      call ftest (sv1,n1,sv2,n2,f,prob)

      if (prob.ge.0.d0) then

      if (prob.lt.5.d-2) then

       call tutest (sv1,n1,sv2,n2,t,prob)

      else

       call ttest (sv1,n1,sv2,n2,t,prob)

      endif

      endif


c     if (prob.lt.1.0d-1) then

c     if (prob.lt.1.0d-2) then



      if (prob.lt.1.0d-3) then




      do i=1,n2
      sv1(i)=sv2(i)
      enddo

      n1=n2

      go to 1

      endif




      return
      end




c
c
c     Subroutine quarte
c
c
c     Automatically finds the quartile lower and higher limits of
c     a pixel sample for the Robust Mean Estimation of that sample
c     or for other average/standard deviation tests.
c
c
c     After sorting the pixel counts in crescent order, the lower
c     and higher quartile pixel limits are set as those for which
c     the line connecting the two pixel counts is closest to an
c     horizontal line, i.e. the count gradient is minimum.
c
c
c
c     ior - variable indicating the crescent order of pixel counts
c     sv  - the pixel sample not ordered in crescent order
c     n   - number of pixels
c     nq  - separation length in pixels between the quartile limits 
c     n1  - lower quartile limit
c     n2  - higher quartile limit
c
c
c
c
c     Last update:  M. Assafin  17/Dec/2021
c
c
c


      subroutine quarte (ida,n,ior,sv,nq,n1,n2)

      IMPLICIT REAL *8 (A-H,O-Z)


      dimension sv(ida),ior(ida)




c
c     The pixel sample is not furnished in crescent
c     count order; the order is set by variable ior
c


      val=+1.d14


c
c     Finds the quartile limits
c


      do m=1,n-nq+1

      mm=m+nq-1

      m1=ior(m)
      m2=ior(mm)

c     tan=dabs(sv(m2)-sv(m1))/(mm-m)

      tan=dabs(sv(m2)-sv(m1))

      if (tan.lt.val) then
      val=tan
      mr=m
      endif

      enddo


c
c     Output quartile limits
c


      n1=mr
      n2=mr+nq-1


      return
      end





c
c
c
c     Subroutine pixapc
c
c
c     Purpose
c
c
c     Returns N pixels inside a given circular aperture, ordered
c     by the pixel distance to the (x,y) aperture center.
c
c
c     The pixel distances to the (x,y) center  are stored in sv1.
c     The pixel counts are stored in sv2.
c
c
c
c     xg,yg = center (pixels)
c
c     n     = output number of pixels that are inside the circular aperture
c
c     sv1   = the pixel distances to the aperture center ordered in crescent
c             order of the pixel distance to the center
c
c     sv2   = the pixel counts ordered in crescent order of the pixel
c             distance to the center
c
c     sc,sp = auxiliary variables
c
c
c
c
c     Last update:   M. Assafin - 01/Mar/2022
c
c
c

      subroutine pixapc (idimx,idimy,ida,pixmat,nx,ny,xg,yg,gann,ior,
     ?sv1,sv2,n)

      implicit real*8 (A-H,O-Z)


      real*4 pixmat(idimx,idimy)

      dimension sc(ida),sp(ida),sv1(ida),sv2(ida),ior(ida)



c
c     Defines the matrix search limits adding 1 pixel
c     for safe margin
c


      jx1=xg-gann
      jx2=xg+gann
      jy1=yg-gann
      jy2=yg+gann

      if (jx1.lt.1) jx1=1
      if (jy1.lt.1) jy1=1

      if (jx2.gt.nx) jx2=nx
      if (jy2.gt.ny) jy2=ny

c

      n=0

      do 2 k=jy1,jy2

      do 1 j=jx1,jx2


      if (pixmat(j,k).lt.0.d0) go to 1

      call circul (gann,xg,yg,j,k,ichave)
      if (ichave.lt.0) go to 1

c     call circol (gann,xg,yg,j,k,ichave)
c     if (ichave.lt.0) go to 1



c
c     Stores pixels inside the circular aperture
c

      n=n+1

      sc(n)=pixmat(j,k)
      sp(n)=dsqrt((j-xg)**2+(k-yg)**2)
      ior(n)=n


  1   continue
  2   continue


c
c     Orders pixels by their distance to the
c     aperture center
c

      call dordem (ida,n,ior,sp)


c
c     Stores pixel distances and counts ordered by
c     the pixel distance to the (x,y) aperture center 
c


      do k=1,n

      i=ior(k)

      sv1(k)=sp(i)
      sv2(k)=sc(i)

      enddo


      return
      end




c
c
c
c     Subroutine pixap
c
c
c     Purpose
c
c
c     Returns the pixel counts of pixels
c     inside a given circular aperture.
c
c
c
c     Last update:   M. Assafin - 20/September/2018
c
c
c

      subroutine pixap (idimx,idimy,ida,pixmat,nx,ny,xg,yg,gann,sv,n)

      implicit real*8 (A-H,O-Z)


      real*4 pixmat(idimx,idimy)

      dimension sv(ida)



c
c     Defines the matrix search limits
c


      jx1=xg-gann
      jx2=xg+gann
      jy1=yg-gann
      jy2=yg+gann


c

      n=0

      do 2 k=jy1,jy2

      if (k.lt.1)  go to 2
      if (k.gt.ny) go to 2

      do 1 j=jx1,jx2

      if (j.lt.1)  go to 1
      if (j.gt.nx) go to 1


      if (pixmat(j,k).lt.0.d0) go to 1

      call circul (gann,xg,yg,j,k,ichave)
      if (ichave.lt.0) go to 1

c
c     Stores pixels inside the circular aperture
c

      n=n+1
      sv(n)=pixmat(j,k)

  1   continue
  2   continue

      return
      end




c
c
c    Subroutine subar
c
c
c    Computes the (x,y) center of an object within a circle following
c    the novel centering algorithm called "Photogravity Center Method",
c    or PGC.
c
c    The usual modified moment method (Stone 1989) only weights the
c    pixel coordinates by the pixel counts. Only pixels above a given
c    threshold cutoff are used.
c
c    Derived from the Modified Moment algorithm (Stone 1989), the
c    Photogravity Center Method (PGC) also weights the used pixels (those
c    above a threshold cutoff) by their inter-distances. The used pixels more
c    concentrated next to each other have more weight than those pixels
c    more distant from the concentration, resembling center of gravity
c    calculations for a non-uniform mass distribution. The new weight
c    goes with the square of the distances.
c
c
c    The advantage of the PGC method over the traditional method is that
c    it is more sensitive to the grouping of real object pixels, for which
c    weights are higher than for the more isolated spurious bright pixels
c    nearby. In this way, the center is improved, particularly for faint
c    objects.
c
c
c    Here, the threshold cutoff is simply defined as the minimum count
c    for the 25% brighter pixels. If only 3 or less pixels are above this
c    threshold, then a new threshold is assumed by taking the brightest 50%
c    pixels. If again only 3 or less pixels are above this threshold, then
c    a final threshold is set by taking the brightest 75% pixels.
c
c
c
c    Called subroutines:
c
c    circol  - selects pixels within a circle; pixels "cut" by the
c              circunference of the circle are considered as pixels
c              within the circle;
c
c    circul  - simplified and faster version of the above subroutine
c
c
c    Variables:
c
c
c    nx, ny          = pixel matrix dimensions
c    pixmat          = pixel matrix
c
c    jx1,jx2,jy1,jy2 = limits of the rectangle area containing the circle
c
c    (xi,yi)         = input (x,y) circle center
c    raio            = circle radius
c
c    (xsb,ysb)       = output (x,y) circle center computed by the
c                      Super-Baricenter method
c
c    sv              = auxiliary variable (flux ordered in crescent order)
c    ior             = auxiliary variable (ordering)
c    ixb, iyb        = auxiliary variables (centering)
c    nb              = number of pixels used in computations,
c                      flagged (negative) pixels excluded
c
c
c
c     Last update: M. Assafin 19/February/2020
c


      subroutine subar (ida,idimx,idimy,sv,ior,ixb,iyb,pixmat,nx,ny,xi,
     ?yi,raio,jx1,jx2,jy1,jy2,xsb,ysb,nb)


      implicit real*8 (a-h,o-z)

      real*4 pixmat(idimx,idimy)


      dimension sv(ida),ior(ida),ixb(ida),iyb(ida)



c
c     Stores counts and coordinates of pixels
c     within the circle
c

      nb=0

      do   i=jy1,jy2
      do 2 j=jx1,jx2

      if (pixmat(j,i).lt.0.d0) go to 2

c     call circol (raio,xi,yi,j,i,ichave)

      call circul (raio,xi,yi,j,i,ichave)

      if (ichave.lt.0) go to 2

      nb=nb+1

      ior(nb)=nb
      sv(nb)=pixmat(j,i)
      ixb(nb)=j
      iyb(nb)=i


 2    continue
      enddo


      if (nb.eq.0) return


c
c     Orders pixels in crescent count order
c

      call dordem (ida,nb,ior,sv)

c 
c     Computes the (x,y) center by the Super-Baricenter method,
c     using the 1/4 brightest available pixels within the circular
c     aperture.
c
c     If there are less than 4 brightest pixels available, 2/4 of the
c     brightest pixels are used at this step, and if still less than
c     4 pixels are left, 3/4 are used.
c
c     The distance weight goes with the square of the distances. For
c     each pixel, the square of the distances are averaged over the
c     number of neighbour pixels, so as to avoid computer overflowing.
c


      xsb=0.d0
      ysb=0.d0

      wcount=0.d0

      nbi=nb*0.75d0

      if (nbi.lt.4) nbi=nb*0.50d0

      if (nbi.lt.4) nbi=nb*0.25d0


      if (nbi.eq.0) nbi=1


      dn=nb-nbi

c
c     Main center loop
c

      do i=nbi,nb

      m=ior(i)

      d=0.d0


c
c     Distance weight loop
c

      do 10 l=nbi,nb

      if (l.eq.i) go to 10

      n=ior(l)
      
      d=d+((ixb(m)-ixb(n))**2+(iyb(m)-iyb(n))**2)/dn


 10   continue

      xsb=xsb+sv(m)*ixb(m)/d
      ysb=ysb+sv(m)*iyb(m)/d

      wcount=wcount+sv(m)/d

      enddo

c
c     (x,y) center by the Super-Baricenter method
c

      xsb=xsb/wcount
      ysb=ysb/wcount


      return
      end




c
c
c    Subroutine bar
c
c
c    Computes the (x,y) center of an object within a circle following
c    a 2D version of the classical Modified Moment method by Stone (1989).
c
c    The usual Modified Moment method (Stone 1989) only weights the
c    pixel coordinates by the pixel counts. Only pixels above a given
c    threshold cutoff are used.
c
c
c    Here, the threshold cutoff is simply defined as the minimum count
c    for the 25% brighter pixels. If only 3 or less pixels are above this
c    threshold, then a new threshold is assumed by taking the brightest 50%
c    pixels. If again only 3 or less pixels are above this threshold, then
c    a final threshold is set by taking the brightest 75% pixels.
c
c
c
c    Called subroutines:
c
c    circol  - selects pixels within a circle; pixels "cut" by the
c              circunference of the circle are considered as pixels
c              within the circle;
c
c    circul  - simplified and faster version of the above subroutine
c
c
c    Variables:
c
c
c    nx, ny          = pixel matrix dimensions
c    pixmat          = pixel matrix
c
c    jx1,jx2,jy1,jy2 = limits of the rectangle area containing the circle
c
c    (xi,yi)         = input (x,y) circle center
c    raio            = circle radius
c
c    (xsb,ysb)       = output (x,y) circle center computed by the
c                      2D Modified Moment method
c
c    sv              = auxiliary variable (flux ordered in crescent order)
c    ior             = auxiliary variable (ordering)
c    ixb, iyb        = auxiliary variables (centering)
c    nb              = number of pixels used in computations,
c                      flagged (negative) pixels excluded
c
c
c
c     Last update: M. Assafin 01/Mar/2022
c


      subroutine bar (ida,idimx,idimy,sv,ior,ixb,iyb,pixmat,nx,ny,xi,yi,
     ?raio,jx1,jx2,jy1,jy2,xsb,ysb,nb)


      implicit real*8 (a-h,o-z)

      real*4 pixmat(idimx,idimy)


      dimension sv(ida),ior(ida),ixb(ida),iyb(ida)



c
c     Stores counts and coordinates of pixels
c     within the circle
c

      nb=0

      do   i=jy1,jy2
      do 2 j=jx1,jx2

      if (pixmat(j,i).lt.0.d0) go to 2

c     call circol (raio,xi,yi,j,i,ichave)

      call circul (raio,xi,yi,j,i,ichave)

      if (ichave.lt.0) go to 2

      nb=nb+1

      ior(nb)=nb
      sv(nb)=pixmat(j,i)
      ixb(nb)=j
      iyb(nb)=i


 2    continue
      enddo


      if (nb.eq.0) return


c
c     Orders pixels in crescent count order
c

      call dordem (ida,nb,ior,sv)

c 
c     Computes the (x,y) center by using the 1/4 brightest available
c     pixels within the circular aperture.
c
c     If there are less than 4 brightest pixels available, 2/4 of the
c     brightest pixels are used at this step, and if still less than
c     4 pixels are left, 3/4 are used.
c
c


      xsb=0.d0
      ysb=0.d0

      count=0.d0

      nbi=nb*0.75d0

      if (nbi.lt.4) nbi=nb*0.50d0

      if (nbi.lt.4) nbi=nb*0.25d0


      if (nbi.eq.0) nbi=1


c
c     Main center loop
c


      do i=nbi,nb

      m=ior(i)

      xsb=xsb+sv(m)*ixb(m)
      ysb=ysb+sv(m)*iyb(m)

      count=count+sv(m)

      enddo

c
c     (x,y) center by the Super-Baricenter method
c

      xsb=xsb/count
      ysb=ysb/count


      return
      end






c
c     Function theta
c
c    
c     Given x and y coordinates, computes the angle theta which
c     is zero toward x and increases counter-clockwise.
c
c
c
c     Last modification: M. Assafin - 17/Dec/2021
c


      double precision function theta (cs,sn)

      implicit real*8 (a-h,o-z)

c

      pi=3.141592653589793238462643d0

c

      aux=dabs(dasin(dabs(sn)/dsqrt((cs)**2+(sn)**2)))


      if (cs.ge.0.d0) then
       if (sn.ge.0.d0) then
        theta=aux
       else
        theta=2.d0*pi-aux
       endif
      else
       if (sn.ge.0.d0) then
        theta=pi-aux
       else
        theta=pi+aux
       endif
      endif


      return
      end





c
c
c     Subroutine momeci
c
c
c     Computes second order moments of pixels within a circular
c     aperture.
c
c     From the second order moments, it also computes the semi-major
c     axis, semi-minor-axis and rotation angle of the semi-major
c     axis with respect to the X axis. From the semi-major and
c     semi-minor axes, the equivalent sigma is also computed.
c
c     These quantities are or may be inputs to the pixel
c     sampling of ellipse rings and PSF fittings.
c
c
c
c     ix1,ix2,
c     iy1,iy2 - limits for pixels in the matrix
c
c     cx, cy  - aperture center
c     gra     - circle radius (pixels)
c
c
c     u20, u11
c     u02     - second central moments (pixel units)
c
c
c     eg1     - semi-major axis (pixels)
c     eg2     - semi-minor axis (pixels)
c     sig     - equivalent sigma (pixels)
c     tet     - rotation angle (radians) 
c
c
c
c
c
c     Last update:  M. Assafin  17/Dec/2021
c
c
c


      subroutine momeci (idimx,idimy,id21,coefx,pixmat,ix1,ix2,iy1,iy2,
     ?cx,cy,gra,u20,u11,u02,eg1,eg2,sig,tet,ecent)

      IMPLICIT REAL *8 (A-H,O-Z)

      real*4 pixmat(idimx,idimy)

      dimension coefx(id21)


c
c     Determines the second order moments of the object
c     and associated semi-major and semi-minor axes, and
c     rotation angle.
c
c

      iraio2=gra**2

      cfun=1.d14
 
      do i=iy1,iy2
      do j=ix1,ix2
      ira=(j-cx)**2+(i-cy)**2
      if (ira.le.iraio2) then
      if (pixmat(j,i).gt.0.d0.and.pixmat(j,i).lt.cfun) cfun=pixmat(j,i)
      endif
      enddo
      enddo


c

      do kkkk=1,6
      coefx(kkkk)=0.d0
      enddo



      do     i=iy1,iy2
      do 270 j=ix1,ix2

      if (pixmat(j,i).lt.0.d0) go to 270

      ira=(j-cx)**2+(i-cy)**2
      if (ira.gt.iraio2) go to 270

      icont=0
      do kk=1,3
      do m=1,kk
      l=kk-m
      icont=icont+1
      coefx(icont)=coefx(icont)+(pixmat(j,i)-cfun)*(j**l)*(i**(m-1))
c     coefx(icont)=coefx(icont)+(pixmat(j,i))*(j**l)*(i**(m-1))
      enddo
      enddo


 270  continue
      enddo


c
c     Computes second order moments
c
c
c     M00 = coefx(1)
c     M10 = coefx(2)
c     M01 = coefx(3)
c     M20 = coefx(4)
c     M11 = coefx(5)
c     M02 = coefx(6)
c
c

      xm=coefx(2)/coefx(1)
      ym=coefx(3)/coefx(1)



c
c     Now the second order central moments of the image
c     normalized by the sum of pixel counts M00
c


      u20=coefx(4)/coefx(1)-xm**2
      u02=coefx(6)/coefx(1)-ym**2
      u11=coefx(5)/coefx(1)-xm*ym


c
c     Computes the semi-major and semi-minor axes, and
c     the excentricity
c


      eg1=(u20+u02)/2.d0+dsqrt(4.d0*u11**2+(u20-u02)**2)/2.d0
      eg2=(u20+u02)/2.d0-dsqrt(4.d0*u11**2+(u20-u02)**2)/2.d0


      ecent=dsqrt(1.d0-eg2/eg1)


      eg1=dsqrt(eg1)
      eg2=dsqrt(eg2)


c
c     Computes the equivalent sigma
c
c
c     For the best S/N aperture of the PROUD method, its radius is
c     large enough that the sigma as computed below is approximatelly
c     equal to the sigma of a 2D Circular Gaussian PSF. 
c


      sig=dsqrt(eg1*eg2)




c
c     Computes the orientatin angle of the semimajor axis with
c     respect to the X axis.
c



       sn=2.d0*u11
       cs=u20-u02

       tet=0.5d0*theta(cs,sn)


       return
       end





c
c
c     Subroutine ftesta
c
c
c     A simplified version of the ftest subroutine of the
c     Numerical Recipes in FORTRAN, 2nd Edition.
c
c     Given the standard deviations of 2 data sets N1 and N2, this
c     subroutine returns the value of f, and its significance as prob.
c     Small values of prob indicate that the two data sets have
c     significantly different variances.
c
c
c
c     Last modification:  M. Assafin   20/Sep/2021
c



      subroutine ftesta (n1,n2,sd1,sd2,f,prob)

      implicit real*8 (a-h,o-z)


      var1=sd1**2
      var2=sd2**2

c     if (sd1.eq.0.d0.and.sd2.eq.0.d0) then
c     f=0.d0
c     prob=-1.d0
c     return
c     endif


      if (sd1.eq.0.d0.or.sd2.eq.0.d0) then
      f=0.d0
c     prob=-1.d0
      prob=0.d0
      return
      endif


      if(sd1.gt.sd2)then

c       if (sd2.eq.0.d0) then
c       f=0.d0
c       prob=-1.d0
c       return
c       endif

        f=var1/var2
        df1=n1-1
        df2=n2-1

      else

c       if (sd1.eq.0.d0) then
c       f=0.d0
c       prob=-1.d0
c       return
c       endif

        f=var2/var1
        df1=n2-1
        df2=n1-1
      endif
      prob=2.d0*betai(0.5d0*df2,0.5d0*df1,df2/(df2+df1*f))
      if(prob.gt.1.d0)prob=2.d0-prob
      return
      END




c
c
c     SUBROUTINE ftest
c
c
c     Numerical Recipes in FORTRAN, 2nd Edition.
c
c     Given the arrays data1(1:n1) and data2(1:n2), this routine returns
c     the value of f, and its significance as prob. Small values of prob
c     indicate that the two arrays have significantly different variances.
c
c
c
c     Last modification:  M. Assafin   20/Sep/2021
c



      SUBROUTINE ftest(data1,n1,data2,n2,f,prob)

      implicit real*8 (a-h,o-z)


c     INTEGER n1,n2
c     REAL f,prob,data1(n1),data2(n2)
C     USES avevar,betai
c     REAL ave1,ave2,df1,df2,var1,var2,betai

      dimension data1(n1),data2(n2)

      call avevar(data1,n1,ave1,var1)
      call avevar(data2,n2,ave2,var2)

c     if (var1.eq.0.d0.and.var2.eq.0.d0) then
c     f=0.d0
c     prob=-1.d0
c     return
c     endif

      if (var1.eq.0.d0.or.var2.eq.0.d0) then
      f=0.d0
c     prob=-1.d0
      prob=0.d0
      return
      endif


      if(var1.gt.var2)then

c       if (var2.eq.0.d0) then
c       f=0.d0
c       prob=-1.d0
c       return
c       endif

        f=var1/var2
        df1=n1-1
        df2=n2-1

      else

c       if (var1.eq.0.d0) then
c       f=0.d0
c       prob=-1.d0
c       return
c       endif

        f=var2/var1
        df1=n2-1
        df2=n1-1
      endif
      prob=2.d0*betai(0.5d0*df2,0.5d0*df1,df2/(df2+df1*f))
      if(prob.gt.1.d0)prob=2.d0-prob
      return
      END



c
c
c     SUBROUTINE ttest
c
c
c     Numerical Recipes in FORTRAN, 2nd Edition.
c
c
c     Given the arrays data1(1:n1) and data2(1:n2), this routine
c     returns Student$B!G(Bs t as t, and its significance as prob, small
c     values of prob indicating that the arrays have significantly
c     different means. The data arrays are assumed to be drawn from
c     populations with the same true variance.
c
c
c     Last modification:  M. Assafin   20/Sep/2021
c
c



      SUBROUTINE ttest(data1,n1,data2,n2,t,prob)

      implicit real*8 (a-h,o-z)

c     INTEGER n1,n2
c     REAL prob,t,data1(n1),data2(n2)

      dimension data1(n1),data2(n2)

C     USES avevar,betai
c     REAL ave1,ave2,df,var,var1,var2,betai
      call avevar(data1,n1,ave1,var1)
      call avevar(data2,n2,ave2,var2)


c     if (var1.eq.0.d0.and.var2.eq.0.d0) then
c     t=0.d0
c     prob=-1.d0
c     return
c     endif


      if (var1.eq.0.d0.or.var2.eq.0.d0) then
      t=0.d0
      prob=0.d0
      return
      endif



c     if (n1.le.1.or.n2.le.1) then
c     t=0.d0
c     prob=-1.d0
c     return
c     endif


      df=n1+n2-2
      var=((n1-1)*var1+(n2-1)*var2)/df
      if(var.ne.0.d0) then
      t=(ave1-ave2)/dsqrt(var*(1.d0/n1+1.d0/n2))
      prob=betai(0.5d0*df,0.5d0,df/(df+t**2))
      else
      t=0.d0
c     prob=-1.d0
      prob=1.d0
      endif
      return
      END


c
c
c
c     SUBROUTINE tutest
c
c
c
c     Numerical Recipes in FORTRAN, 2nd Edition.
c
c     Given the arrays data1(1:n1) and data2(1:n2), this routine
c     returns Student$B!G(Bs t as t, and its significance as prob, small values
c     of prob indicating that the arrays have significantly different means.
c     The data arrays are allowed to be drawn from populations with unequal
c     variances.
c
c
c     Last modification:  M. Assafin   20/Sep/2021
c
c


      SUBROUTINE tutest(data1,n1,data2,n2,t,prob)

      implicit real*8 (a-h,o-z)

c     INTEGER n1,n2
c     REAL prob,t,data1(n1),data2(n2)

      dimension data1(n1),data2(n2)


C     USES avevar,betai
c     REAL ave1,ave2,df,var1,var2,betai


      call avevar(data1,n1,ave1,var1)
      call avevar(data2,n2,ave2,var2)

c     if (var1.eq.0.d0.and.var2.eq.0.d0) then
c     t=0.d0
c     prob=-1.d0
c     return
c     endif

      if (var1.eq.0.d0.or.var2.eq.0.d0) then
      t=0.d0
      prob=0.d0
      return
      endif



      if (n1.le.1.or.n2.le.1) then
      t=0.d0
c     prob=-1.d0
      prob=1.d0
      return
      endif

      t=(ave1-ave2)/dsqrt(var1/n1+var2/n2)
      

      df=(var1/n1+var2/n2)**2/((var1/n1)**2/(n1-1)+(var2/n2)**2/(n2-1))
      prob=betai(0.5d0*df,0.5d0,df/(df+t**2))
      return
      END



c
c
c     SUBROUTINE avevar
c
c
c     Numerical Recipes in FORTRAN, 2nd Edition.
c
c     Given array data(1:n), returns its mean as ave and
c     its variance as var.
c
c
c     Last modification:  M. Assafin   20/September/2018
c
c

      SUBROUTINE avevar(data,n,ave,var)

      implicit real*8 (a-h,o-z)


c     INTEGER n
c     REAL ave,var,data(n)
c     INTEGER j
c     REAL s,ep

      dimension data(n)


      ave=0.0d0
      do 11 j=1,n
        ave=ave+data(j)
11    continue
      ave=ave/n
      var=0.0d0
      ep=0.0d0
      do 12 j=1,n
        s=data(j)-ave
        ep=ep+s
        var=var+s*s
12    continue

      if (n.gt.1) then
       var=(var-ep**2/n)/(n-1.d0)
      else
       var=0.d0
      endif

      return
      END



c
c
c     Function betai
c
c
c     Numerical Recipes in FORTRAN, 2nd Edition.
c
c     Returns the incomplete beta function Ix (a, b).
c
c
c     Last modification:  M. Assafin   20/September/2018
c

      FUNCTION betai(a,b,x)

      implicit real*8 (a-h,o-z)


c     REAL betai,a,b,x
C     USES betacf,gammln
c     REAL bt,betacf,gammln
      if(x.lt.0.d0.or.x.gt.1.d0) stop
      if(x.eq.0.d0.or.x.eq.1.d0)then
       bt=0.d0
      else
       bt=dexp(gammln(a+b)-gammln(a)-gammln(b)+a*dlog(x)+b*dlog(1.d0-x))
      endif
      if(x.lt.(a+1.d0)/(a+b+2.d0))then
        betai=bt*betacf(a,b,x)/a
        return
      else
        betai=1.d0-bt*betacf(b,a,1.d0-x)/b
        return
      endif

      return
      END




c
c
c     Function betacf
c
c
c     Numerical Recipes in FORTRAN, 2nd Edition.
c
c      Evaluates continued fraction for incomplete beta function
c      by modified Lentz's method 
c
c
c     Last modification:  M. Assafin   20/September/2018
c


      FUNCTION betacf(a,b,x)

      implicit real*8 (a-h,o-z)


c     INTEGER MAXIT
c     REAL betacf,a,b,x,EPS,FPMIN

      PARAMETER (MAXIT=100,EPS=3.d-7,FPMIN=1.d-30)

c     INTEGER m,m2
c     REAL aa,c,d,del,h,qab,qam,qap
      qab=a+b
      qap=a+1.d0
      qam=a-1.d0
      c=1.d0
      d=1.d0-qab*x/qap
      if(dabs(d).lt.FPMIN)d=FPMIN
      d=1.d0/d
      h=d
      do 11 m=1,MAXIT
        m2=2*m
        aa=m*(b-m)*x/((qam+m2)*(a+m2))
        d=1.d0+aa*d
        if(dabs(d).lt.FPMIN)d=FPMIN
        c=1.d0+aa/c
        if(dabs(c).lt.FPMIN)c=FPMIN
        d=1.d0/d
        h=h*d*c
        aa=-(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
        d=1.d0+aa*d
        if(dabs(d).lt.FPMIN)d=FPMIN
        c=1.d0+aa/c
        if(dabs(c).lt.FPMIN)c=FPMIN
        d=1.d0/d
        del=d*c
        h=h*del
        if(dabs(del-1.d0).lt.EPS)goto 1
11    continue
      stop
c     pause 'a or b too big, or MAXIT too small in betacf'
1     betacf=h
      return
      END




c
c
c     Function gammln
c
c
c     Numerical Recipes in FORTRAN, 2nd Edition.
c
c      Returns the value ln[$B&#(B(xx)] for xx > 0.
c
c
c     Last modification:  M. Assafin   20/September/2018
c


      double precision function gammln(xx)

      implicit real*8 (a-h,o-z)

c     REAL gammln,xx
c     INTEGER j
      DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,
     *24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,
     *-.5395239384953d-5,2.5066282746310005d0/
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*dlog(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11    continue
      gammln=tmp+dlog(stp*ser/x)
      return
      END


c
c     Subroutine avsdev
c
c
c     Computes the average and standard deviation of the furnished data
c
c    
c     Comments
c
c
c     contag = data points
c     xm     = average of data points
c     xs     = standard deviation of the data points
c
c
c     Last modified: 27 Sep 2014
c
c
c


      subroutine avsdev (idiobs,n,contag,xm,xs)

      implicit real *8 (a-h,o-z)


      dimension contag(idiobs)


c
c     The simple statistics
c

      
      xm=0.d0
      xs=0.d0
      

      do i=1,n
      xm=xm+contag(i)
      xs=xs+contag(i)**2
      enddo

      call desvio (n,xm,xs)

      if (n.eq.1) xs=0.d0
 

      return
      end




c
c     Subroutine sperim
c
c
c     Loads the counts at a given ring perimeter of width lper. 
c
c     The ring is centered at the furnished (x,y) coordindates
c     with inner radius defined by ilax & ilay.
c
c     
c     Comments
c
c
c     jx1,jx2,jy1,jy2 = coordinates of the square box corners containing
c                       the ring
c     ilax, ilay      = inner ring radius   
c     lper            = width of the ring perimeter
c     pixmat          = pixel matrix
c     imagem          = auxiliary pixel matrix with marked pixels for elimination
c     contag          = the output; counts inside the ring perimeter
c     n               = number of points inside the ring perimeter
c
c
c     Last modified: 27 Sep 2014
c
c
c



      subroutine sperim (idimx,idimy,idiobs,nx,ny,jm,im,ilax,ilay,lper,
     ?pixmat,imagem,contag,n,jx1,jx2,jy1,jy2)

      implicit real *8 (a-h,o-z)

      integer*2 imagem(idimx,idimy)
      real*4 pixmat(idimx,idimy),mazi


      dimension contag(idiobs)

c

      n=0

      nlimo=idiobs

c

      irmin2=ilax**2+ilay**2
      irmax2=(ilax+lper)**2+(ilay+lper)**2


c
c     Sets each valid perimeter
c

      ix1=jm-ilax-lper
      ix2=jm+ilax+lper
      iy1=im-ilay-lper
      iy2=im+ilay+lper


      if (ix1.lt.1)  ix1=1
      if (iy1.lt.1)  iy1=1
      if (ix2.gt.nx) ix2=nx
      if (iy2.gt.ny) iy2=ny


c
c     Picks up counts within the ring of radius sqrt(ilax**2+ilay**2) and
c     width lper
c
      

      do    ii=iy1,iy2
      do 10 jj=ix1,ix2

      if (imagem(jj,ii).lt.-19) go to 10

      ir2=(jj-jm)**2+(ii-im)**2
      if (ir2.lt.irmin2) go to 10
      if (ir2.gt.irmax2) go to 10

      n=n+1
      contag(n)=pixmat(jj,ii)
      if (n.eq.nlimo) go to 50

 10   continue
      enddo

c

 50   continue

c

      jx1=ix1
      jx2=ix2
      jy1=iy1
      jy2=iy2


      return
      end




c
c     Subroutine quartl
c
c
c     Eliminates possible outlier counts (lower and higher counts)
c
c     
c     Comments
c
c
c     contag = the input/output of counts
c     n      = the input/output number of counts
c
c
c     Last modified:  M. Assafin   19/Jan/2019
c
c
c



      subroutine quartl (idiobs,ior,n,perc,contag)


      implicit real *8 (a-h,o-z)

      dimension contag(idiobs),contbo(idiobs),ior(idiobs)


c
c     Validate data
c

      if (n.eq.1) return


c
c     Stores and prepares data
c

      do i=1,n
      ior(i)=i
      contbo(i)=contag(i)
      enddo

c
c     Orders data from low to high counts
c

      call dordem (idiobs,n,ior,contbo)


c
c     Sets lower and higher count limits
c

      n1=perc*n
      n2=n-n1

      if (n1.lt.1) n1=1
      if (n2.lt.1) n2=1



c
c     Fills in selected counts
c


      m=0

      do i=n1,n2
      k=ior(i)
      m=m+1
      contag(m)=contbo(k)
      enddo

      n=m

      return

      end




c
c     Subroutine subtss
c
c
c     Gets the pixel counts of the right/letf/top/botom section of the current
c     rectangle of the current trace-shaped structure
c
c
c
c     Last modified: M. Assafin, 16 Oct 2014
c
c


      subroutine subtss (idimx,idimy,idiobs,pixmat,imagem,nx,ny,ixc,iyc,
     ?ang,j1,j2,i1,i2,n,contag)


      implicit real *8 (a-h,o-z)


      integer*2 imagem(idimx,idimy)
      real*4 pixmat(idimx,idimy),mazi


      dimension contag(idiobs)




c
c     Left/right/top/botom counts of ideal rectangle
c



      n=0

      do    ii=i1,i2
      do 20 jj=j1,j2


      call irot (jj,ii,ixc,iyc,ang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 20

      if (imagem(j,i).lt.-19) go to 20

      n=n+1
      contag(n)=pixmat(j,i)

 20   continue
      enddo


      return

      end

  





c
c
c     Subroutine idtrack
c
c
c     Identifies trace-shaped objects.
c
c     Input comes from candidates detected by the ident subroutine. The candidate
c     objects are filtered and those with potential to be parts of, or a trace,
c     are dropped from the initial list (rounded-shaped objects) and processed
c     for the identification of the trace.  
c
c
c
c     Last modified: M. Assafin, 01/Mar/2022
c
c


      subroutine idtrack (idimx,idimy,idiobs,icofsp,coefx,xest,yest,xp,
     ?pixmat,imagem,nx,ny,contag,fotceu,lper,iflag,ior,nval,xid,yid,
     ?idx1,idx2,idy1,idy2,idlado,npix,bcg,bcgs,nbcg,dan,exce,sige,snr,
     ?cf,cra,cann,uu20,uu02,uu11,seg1,seg2,nstar,elimt,xctra,yctra,
     ?dmalat,dmilat,agtra,bcgt,bcgst,ntrac,u,v,w)



      implicit real *8 (a-h,o-z)

      integer*2 imagem(idimx,idimy)
      real*4 pixmat(idimx,idimy)


      dimension u(idiobs,icofsp),v(icofsp,icofsp),w(icofsp)



      dimension xid(idiobs),yid(idiobs),idlado(idiobs),idx1(idiobs),
     ?idx2(idiobs),idy1(idiobs),idy2(idiobs),npix(idiobs),bcg(idiobs),
     ?bcgs(idiobs),nbcg(idiobs),dan(idiobs),exce(idiobs),iflag(idiobs),
     ?sige(idiobs),snr(idiobs),cf(idiobs),cra(idiobs),uu20(idiobs),
     ?uu02(idiobs),uu11(idiobs),seg1(idiobs),seg2(idiobs),cann(idiobs)

      dimension xctra(idiobs),yctra(idiobs),dmalat(idiobs),
     ?dmilat(idiobs),agtra(idiobs),bcgt(idiobs),bcgst(idiobs),
     ?npixt(idiobs),kflag(idiobs)

      dimension coefx(icofsp),xest(idiobs),yest(idiobs),xp(idiobs),
     ?contag(idiobs),ior(idiobs),nval(idiobs),iora(idiobs)

      character*30 nov

      exc(a,b,c,d,e,f,n)=dsqrt((2.d0*dsqrt((a-c)**2+b**2))/(n*(a+c)+
     ?dsqrt((a-c)**2+b**2)))

c     det33(a11,a12,a13,a21,a22,a23,a31,a32,a33)=a11*((a22*a33)-(a23*a32
c    ?))-a12*((a21*a33)-(a23*a31))+a13*((a21*a32)-(a22*a31))

      det33(a11,a12,a13,a21,a22,a23,a31,a32,a33)=a11*a22*a33+a12*a23*a31
     ?+a13*a21*a32-a31*a22*a13-a32*a23*a11-a33*a21*a12



c
c     Initial values
c

      pi=3.141592653589793D0
      grarad=pi/180.d0
      radgra=180.d0/pi

c

      perc=0.2d0

      dong=0.10d0

      loops=500

      index=2

      nterms=6

c

      do i=1,idiobs
      iflag(i)=0
      kflag(i)=0
      enddo

c
c     Orders the regions by the number of pixels above the sky background theshold
c     (priority is to process regions of brighter objects first) 
c


      do k=1,nstar
      iora(k)=k
      nval(k)=npix(k)
      enddo



      call ordem (idiobs,nstar,iora,nval)
       


c
c     Loops the rounded-shaped candidate object regions
c

      ntrac=0



      write (66,274) 
 274  format('# Filename: traco')

      write (66,276)
 276  format('global color=green dashlist=8 3 width=1 font="helvetica 10
     ? normal roman" select=1 highlite=1 dash=0 fixed=0 edit=1 move=1 de
     ?lete=1 include=1 source=1')

      write (66,277)
 277  format('image')


      write (666,274)
      write (666,276)
      write (666,277)



c


      do 40 lll=nstar,1,-1

      kkk=iora(lll)

c


      if (exce(kkk).lt.elimt) go to 40

      if (iflag(kkk).ne.0) go to 40

c
c
c     A part or all of a trace-shaped object, or trace-shaped structure,
c     or tss for simplicity, was found.
c
c     Now it comes the procedure to find the (rotated) rectangle which contains
c     the entire trace-shaped object. This is made in an interactive procedure.
c
c     First, using the center and rotation angle from the input list, the tss is
c     expanded along each semi-width by lper steps until the sky background is
c     reached on each side (here, care is taken with possible nearby tss). 
c
c     Next, the trace lenght is searched in lper steps until it reaches the
c     sky background too. The rectangle is further improved by repeating the steps
c     above N times (limited to 500 loops) until convergence in (x,y) center,
c     rotation angle and sizes (width and length) are reached.
c     
c     The center, sides and rotation angle of the rectangle are stored, as well as
c     the sky background (average and sigma) and the number of pixels above the sky
c     background threshold.
c
c
c



 5    continue



c
c     Initial values from the input list
c

      npx=npix(kkk)

      ang=dan(kkk)


      xc=xid(kkk)
      yc=yid(kkk)

      ixc=xc
      iyc=yc


      xm=bcg(kkk)
      sb=bcgs(kkk)


      ftsb=xm+fotceu*sb




c
c     More initial values
c


      ipass=0

      dang=1.d14

      lax=-100
      lay=-100

      ixcen=-100
      iycen=-100
 
c
     

 7    continue



      ipass=ipass+1


      nle1=0
      nle2=0

      nwi1=0
      nwi2=0

      itouch=0


c
c     Width estimation of the rectangle
c


 10   continue


      j1=-lper/2.d0-0.2
      j2=+lper/2.d0+0.2

c     j1=-lper/2.d0+0.2
c     j2=+lper/2.d0-0.2




c
c     Rectangle counts at internal bottom
c

 12   continue

      i1=-lper/2.d0-lper*nwi1-0.2
      i2=+lper/2.d0-lper*nwi1+0.2


      call subtss (idimx,idimy,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,
     ?j1,j2,i1,i2,n,contag)


c
c     Statistics of the internal bottom of rectangle
c


      if (n.lt.2) go to 15

      call quartl (idiobs,ior,n,perc,contag)

      call avsdev (idiobs,n,contag,ym1i,ys1i)


c
c     Rectangle counts at external bottom of rectangle
c


      i1=-lper/2.d0-lper*(nwi1+1)-0.2
      i2=+lper/2.d0-lper*(nwi1+1)+0.2


      call subtss (idimx,idimy,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,
     ?j1,j2,i1,i2,n,contag)


c
c     Statistics of the external bottom of rectangle
c


      if (n.lt.2) go to 15

      call quartl (idiobs,ior,n,perc,contag)

      call avsdev (idiobs,n,contag,ym1e,ys1e)

c
c     Touched another nearby tss ?
c

      dif=ym1e-ym1i
      
      if (dif.gt.0.d0) then 
      itouch=1
      go to 15
      endif


c
c     Sky background reached at bottom of the rectangle?
c


c     ftys1e=fotceu*ys1e

c     dif=dabs(dif)

c     if (dif.lt.ftys1e) go to 15

c     if (dif.lt.ftsb) go to 15


      if (ym1i.le.ftsb) go to 15


      nwi1=nwi1+1

      go to 12



c
c     Rectangle counts at internal top
c

 15   continue


      i1=-lper/2.d0+lper*nwi2-0.2
      i2=+lper/2.d0+lper*nwi2+0.2


      call subtss (idimx,idimy,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,
     ?j1,j2,i1,i2,n,contag)


c
c     Statistics of the internal top of rectangle
c


      if (n.lt.2) go to 17

      call quartl (idiobs,ior,n,perc,contag)

      call avsdev (idiobs,n,contag,ym2i,ys2i)


c
c     Rectangle counts at external top of rectangle
c


      i1=-lper/2.d0+lper*(nwi2+1)-0.2
      i2=+lper/2.d0+lper*(nwi2+1)+0.2


      call subtss (idimx,idimy,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,
     ?j1,j2,i1,i2,n,contag)


c
c     Statistics of the external top of rectangle
c


      if (n.lt.2) go to 17

      call quartl (idiobs,ior,n,perc,contag)

      call avsdev (idiobs,n,contag,ym2e,ys2e)


c
c     Touched another nearby tss ?
c

      dif=ym2e-ym2i

      if (dif.gt.0.d0) then 
      itouch=1
      go to 17
      endif

c
c     Sky background reached at top of the rectangle?
c


c     ftys2e=fotceu*ys2e

c     dif=dabs(ym2e-ym2i)

c     if (dif.lt.ftys2e) go to 17

c     if (dif.lt.ftsb) go to 17

      if (ym2i.le.ftsb) go to 17



      nwi2=nwi2+1

      go to 15


c
c     Local sky background threshold from the more trusty width counts statistics
c
c
c     Picks up the smaller of the semi-widths to avoid contamination by
c     other nearby tss
c



 17   continue



c     if (ym1e.le.ym2e) then
c     xm=ym1e
c     sb=ys1e
c     else
c     xm=ym2e
c     sb=ys2e
c     endif
c
c     ftsb=xm+fotceu*sb
c
c     nwi=min0(nwi1,nwi2)



c     if (ym1e.ge.ym2e) then
c     xm=ym1e
c     sb=ys1e
c     else
c     xm=ym2e
c     sb=ys2e
c     endif
c
c     nwi=max0(nwi1,nwi2)


      if (itouch.ne.0) then
c     write (*,*)'itouch 1, xc, yc = ',ixc,iyc
      endif

      if (itouch.ne.0) then

      nwi=min0(nwi1,nwi2)

      else

      nwi=max0(nwi1,nwi2)

      endif

c     nwi=max0(nwi1,nwi2)


c
c     Length estimation of the rectangle
c




 20   continue




      i1=-lper/2.d0-0.2
      i2=+lper/2.d0+0.2


c     i1=-lper/2.d0+0.2
c     i2=+lper/2.d0-0.2



c
c     Rectangle counts at left of the rectangle
c


 22   continue

      j1=-lper/2.d0-lper*nle1-0.2
      j2=+lper/2.d0-lper*nle1+0.2


      call subtss (idimx,idimy,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,
     ?j1,j2,i1,i2,n,contag)


c
c     Statistics of the left of rectangle
c


      if (n.lt.2) go to 25

      call quartl (idiobs,ior,n,perc,contag)

      call avsdev (idiobs,n,contag,xm1,xs1)



c
c     Sky background reached at left of the rectangle?
c



      if (xm1.le.ftsb) go to 25




      nle1=nle1+1

      go to 22



c
c     Rectangle counts at right of the rectangle
c

 25   continue


      j1=-lper/2.d0+lper*nle2-0.2
      j2=+lper/2.d0+lper*nle2+0.2


      call subtss (idimx,idimy,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,
     ?j1,j2,i1,i2,n,contag)


c
c     Statistics of the right of rectangle
c


      if (n.lt.2) go to 30

      call quartl (idiobs,ior,n,perc,contag)

      call avsdev (idiobs,n,contag,xm2,xs2)



c
c     Sky background reached at right of the rectangle?
c




      if (xm2.le.ftsb) go to 30



      nle2=nle2+1


      go to 25



c


 30   continue



c
c     Touched another nearby tss ?
c

      itouch=0


c
c     Rectangle counts at left of the rectangle's end
c


      j1=-lper/2.d0-lper*(nle1+1)-0.2
      j2=+lper/2.d0-lper*(nle1+1)+0.2


      call subtss (idimx,idimy,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,
     ?j1,j2,i1,i2,n,contag)


c
c     Statistics of the external left of rectangle's end
c


      if (n.lt.2) go to 31

      call quartl (idiobs,ior,n,perc,contag)

      call avsdev (idiobs,n,contag,xm1e,xs1e)


      dif=xm1e-xm1

      if (dif.gt.0.d0) then 
      itouch=1
      endif



c
c     Rectangle counts at right of the rectangle's end
c



 31   continue


      j1=-lper/2.d0+lper*(nle2+1)-0.2
      j2=+lper/2.d0+lper*(nle2+1)+0.2

      call subtss (idimx,idimy,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,
     ?j1,j2,i1,i2,n,contag)



c
c     Statistics of the external right of rectangle's end
c




      if (n.lt.2) go to 32

      call quartl (idiobs,ior,n,perc,contag)

      call avsdev (idiobs,n,contag,xm2e,xs2e)


      dif=xm2e-xm2

      if (dif.gt.0.d0) then 
      itouch=1
      endif


c

 32   continue


      if (itouch.ne.0) then

      nle=min0(nle1,nle2)

      else

      nle=max0(nle1,nle2)

      endif

c



      j1=-lper*nle
      j2=+lper*nle


      i1=-lper*nwi 
      i2=+lper*nwi 


      ladx=j2-j1+1
      lady=i2-i1+1




c
c     Convergence reached? 
c


      deng=dabs(ang-dang)*radgra


      if (ladx.eq.lax.and.lady.eq.lay.and.ixc.eq.ixcen.and.iyc.eq.iycen.
     ?and.deng.le.dong)  go to 34





      if (ipass.eq.loops) go to 34

      lax=ladx
      lay=lady

      ixcen=ixc
      iycen=iyc

      dang=ang



c
c
c     Computes new center and new rotation angle using
c     the pixels within the current rectangle region.
c
c     The angle theta is computed from the fitting of an unweighted
c     3D cartesian conic function:
c
c
c      ADU counts = matrix(x,y) = Ax^2+Bxy+Cy^2+Dx+Ey+F
c
c
c     by using:
c
c
c                     theta = 0.5 arctg [B/(A-C)]
c
c
c
c     The center is computed by the baricenter of weighted pixels
c     above the sky background threshold.
c
c





      n=0

c     cont=0.d0
c     xc=0.d0
c     yc=0.d0


c     write (*,*) 'lll,ftsb = ',lll,ftsb


      do kkkk=1,nterms
      coefx(kkkk)=0.d0
      enddo



      do    ii=i1,i2
      do 33 jj=j1,j2

      call irot (jj,ii,ixc,iyc,ang,nx,ny,j,i,iout)


      if (iout.ne.0) go to 33

      if (imagem(j,i).lt.-19) go to 33

      if (pixmat(j,i).lt.ftsb) go to 33


      n=n+1


c     xest(n)=j
c     yest(n)=i

c     contag(n)=pixmat(j,i)

c     xp(n)=1.d0


c     contag(n)=1.d0
c     contag(n)=sqrt(pixmat(j,i))

c     xp(n)=pixmat(j,i)

c     xc=xc+j*pixmat(j,i)
c     yc=yc+i*pixmat(j,i)
c     cont=cont+pixmat(j,i)


      icont=0
      do kk=1,3
      do m=1,kk
      l=kk-m
      icont=icont+1
      coefx(icont)=coefx(icont)+pixmat(j,i)*(j**l)*(i**(m-1))
      enddo
      enddo


 33   continue
      enddo

c     if (n.eq.0) write (*,*) 'n=0 lll,kkk = ',lll,kkk

c     xc=xc/cont
c     yc=yc/cont

      npx=n


      xc=coefx(2)/coefx(1)
      yc=coefx(3)/coefx(1)

      u20=coefx(4)/coefx(1)-xc**2
      u02=coefx(6)/coefx(1)-yc**2
      u11=coefx(5)/coefx(1)-xc*yc

c     ang=0.5d0*datan2(2.d0*u11,u20-u02)


      ixc=xc
      iyc=yc



c
c     Fits the 3D unweighted conic
c


c     ngrau=2
c
c     call isolo (idiobs,icofsp,ngrau,n,xest,yest,xp,contag,coefx,
c    ?ierro)



c     call svdfit (xest,yest,xp,contag,n,coefx,nterms,u,v,w,idiobs,
c    ?icofsp,chisqx,idiobs,icofsp,index)



c     a=coefx(4)
c     b=coefx(5)
c     c=coefx(6)



c
c     Computes the angle
c
c


c     sn=b
c     cs=a-c
c

      sn=2.d0*u11
      cs=u20-u02
 
      ayy=dabs(sn)
      axx=dabs(cs)
 
      if (axx.gt.1.d-14) then
 
 
      aux=dabs(datan2(ayy,axx))
 
 
      if (cs.ge.0.d0) then
       if (sn.ge.0.d0) then
        tet=aux
       else
        tet=2.d0*pi-aux
       endif
      else
       if (sn.ge.0.d0) then
        tet=pi-aux
       else
        tet=pi+aux
       endif
      endif
 
      else
 
      if (sn.ge.0.d0) then
      tet=pi/2.d0
      else
      tet=pi+pi/2.d0
      endif
 
      endif
 
 
      ang=0.5d0*tet



c


      go to 7

c
 
   
 34   continue


c
c     Stores trace-shaped object rectangle data
c



      nov=''
      write(nov,*) xc
      do mm=1,28
      if (nov(mm:mm+2).eq.'NaN') then
      iflag(kkk)=1
      go to 40
      endif
      enddo

      nov=''
      write(nov,*) yc
      do mm=1,28
      if (nov(mm:mm+2).eq.'NaN') then
      iflag(kkk)=1
      go to 40
      endif
      enddo

c


      ixc=xc
      iyc=yc

      ntrac=ntrac+1

      xctra(ntrac)=xc
      yctra(ntrac)=yc
      dmalat(ntrac)=ladx
      dmilat(ntrac)=lady
      agtra(ntrac)=ang
      bcgt(ntrac)=xm
      bcgst(ntrac)=sb
      npixt(ntrac)=npx





      write (666,116) xctra(ntrac),yctra(ntrac),dmalat(ntrac),
     ?dmilat(ntrac),agtra(ntrac)*radgra




c     write (*,*) 'kkk,n,dan,ang = ',kkk,ntrac,dan(kkk)*radgra,
c    ?ang*radgra,ixc,iyc,xc,yc,ipass



c
c     Checks and marks other circular candidate object regions of the input
c     list that fall inside the rectangle region of the identified traced-shape
c     object. It mainly avoids that other circular regions are processed over
c     and over again against the same rectangular region.
c


      iflag(kkk)=1


      ang=-ang

      ddma=dmalat(ntrac)/2.d0
      ddmi=dmilat(ntrac)/2.d0


      do 35 k=1,nstar

      if (iflag(k).ne.0) go to 35


      jj=xid(k)-ixc
      ii=yid(k)-iyc


      call irot (jj,ii,ixc,iyc,ang,nx,ny,j,i,iout)


      if (iout.ne.0) go to 35

      dx=j-ixc
      dy=i-iyc

      dx=dabs(dx)
      dy=dabs(dy)


      if (dx.le.ddma .and. dy.le.ddmi) iflag(k)=1

     

 35   continue

c

 40   continue   



c
c     Eliminates trace-shaped rectangles that fall inside larger
c     detected trace-shaped rectangular regions
c


      do  k=1,ntrac

      ixc=xctra(k)
      iyc=yctra(k)
      ang=-agtra(k)
      ddma=dmalat(k)/2.d0
      ddmi=dmilat(k)/2.d0
      npx=npixt(k)

      do 42 m=1,ntrac

      if (k.eq.m) go to 42

      jj=xctra(m)-ixc
      ii=yctra(m)-iyc


      call irot (jj,ii,ixc,iyc,ang,nx,ny,j,i,iout)


      if (iout.ne.0) go to 42


      dx=j-ixc
      dy=i-iyc

      dx=dabs(dx)
      dy=dabs(dy)


      if (dx.le.ddma .and. dy.le.ddmi) then

      if (npx.lt.npixt(m)) then
      kflag(k)=1
      else
      kflag(m)=1
      endif

      endif

 42   continue

      enddo

c


      nt=ntrac
      ntrac=0


      do 43 k=1,nt


      if (kflag(k).ne.0) go to 43

      ntrac=ntrac+1

      xctra(ntrac)=xctra(k)
      yctra(ntrac)=yctra(k)
      dmalat(ntrac)=dmalat(k)
      dmilat(ntrac)=dmilat(k)
      agtra(ntrac)=agtra(k)
      bcgt(ntrac)=bcgt(k)
      bcgst(ntrac)=bcgst(k)
      npixt(ntrac)=npixt(k)



      write (66,116) xctra(ntrac),yctra(ntrac),dmalat(ntrac),
     ?dmilat(ntrac),agtra(ntrac)*radgra
 116  format('box(',4(f8.2,','),f8.2,')')



 43   continue



c
c     Eliminates rounded-shaped candidates that eventually still falls
c     inside the detected trace-shaped rectangular regions
c


      nest=nstar

      nstar=0


      do 50 k=1,nest


      if (iflag(k).ne.0) go to 50


      do 45 m=1,ntrac


      ixc=xctra(m)
      iyc=yctra(m)
      ang=-agtra(m)


      jj=xid(k)-ixc
      ii=yid(k)-iyc


      call irot (jj,ii,ixc,iyc,ang,nx,ny,j,i,iout)


      if (iout.ne.0) go to 45


      dx=j-ixc
      dy=i-iyc

      dx=dabs(dx)
      dy=dabs(dy)


      ddma=dmalat(m)/2.d0
      ddmi=dmilat(m)/2.d0


      if (dx.le.ddma .and. dy.le.ddmi) go to 50


 45   continue



      nstar=nstar+1

      xid(nstar)=xid(k)
      yid(nstar)=yid(k)
      idlado(nstar)=idlado(k)

      idx1(nstar)=idx1(k)
      idx2(nstar)=idx2(k)
      idy1(nstar)=idy1(k)
      idy2(nstar)=idy2(k)

      npix(nstar)=npix(k)

      bcg(nstar)=bcg(k)
      bcgs(nstar)=bcgs(k)
      nbcg(nstar)=nbcg(k)

      dan(nstar)=dan(k)
      exce(nstar)=exce(k)
      sige(nstar)=sige(k)
      seg1(nstar)=seg1(k)
      seg2(nstar)=seg2(k)
      snr(nstar)=snr(k)
      cf(nstar)=cf(k)
      cra(nstar)=cra(k)
      cann(nstar)=cann(k)
      uu20(nstar)=uu20(k)
      uu02(nstar)=uu02(k)
      uu11(nstar)=uu11(k)


 50   continue


c     stop

      return

      end









c
c     Subroutine iau_jd2cal
c
c


      SUBROUTINE iau_jd2cal ( DJ1, DJ2, IY, IM, ID, FD, J )
*+
*  - - - - - - - - - - -
*   i a u _ J D 2 C A L
*  - - - - - - - - - - -
*
*  Julian Date to Gregorian year, month, day, and fraction of a day.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     DJ1,DJ2     d     Julian Date (Notes 1, 2)
*
*  Returned:
*     IY          i     year
*     IM          i     month
*     ID          i     day
*     FD          d     fraction of day
*     J           i     status:
*                           0 = OK
*                          -1 = unacceptable date (Note 3)
*
*  Notes:
*
*  1) The earliest valid date is -68569.5 (-4900 March 1).  The
*     largest value accepted is 10^9.
*
*  2) The Julian Date is apportioned in any convenient way between
*     the arguments DJ1 and DJ2.  For example, JD=2450123.7 could
*     be expressed in any of these ways, among others:
*
*             DJ1            DJ2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*  3) In early eras the conversion is from the "Proleptic Gregorian
*     Calendar";  no account is taken of the date(s) of adoption of
*     the Gregorian Calendar, nor is the AD/BC numbering convention
*     observed.
*
*  Reference:
*
*     Explanatory Supplement to the Astronomical Almanac,
*     P.Kenneth Seidelmann (ed), University Science Books (1992),
*     Section 12.92 (p604).
*
*  This revision:  2000 December 19
*
*  Copyright (C) 2001 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DJ1, DJ2
      INTEGER IY, IM, ID
      DOUBLE PRECISION FD
      INTEGER J

*  Minimum and maximum allowed JD
      DOUBLE PRECISION DJMIN, DJMAX
      PARAMETER ( DJMIN = -68569.5D0, DJMAX = 1D9 )

      INTEGER JD, L, N, I
      DOUBLE PRECISION DJ, D1, D2, F1, F2, F, D

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Check if date is acceptable.
      DJ = DJ1 + DJ2
      IF ( DJ.LT.DJMIN .OR. DJ.GT.DJMAX ) THEN
         J = -1
      ELSE
         J = 0

*     Copy the date, big then small, and re-align to midnight.
         IF ( DJ1 .GE. DJ2 ) THEN
            D1 = DJ1
            D2 = DJ2
         ELSE
            D1 = DJ2
            D2 = DJ1
         END IF
         D2 = D2 - 0.5D0

*     Separate day and fraction.
         F1 = MOD(D1,1D0)
         F2 = MOD(D2,1D0)
         F = MOD(F1+F2,1D0)
         IF ( F .LT. 0D0 ) F = F+1D0
         D = ANINT(D1-F1) + ANINT(D2-F2) + ANINT(F1+F2-F)
         JD = NINT(D) + 1

*     Express day in Gregorian calendar.
         L = JD + 68569
         N = ( 4*L ) / 146097
         L = L - ( 146097*N + 3 ) / 4
         I = ( 4000 * (L+1) ) / 1461001
         L = L - ( 1461*I ) / 4 + 31
         J = ( 80*L ) / 2447
         ID = L - ( 2447*J ) / 80
         L = J / 11
         IM = J + 2 - 12*L
         IY = 100 * ( N-49 ) + I + L

         FD = F
         J = 0
      END IF

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2001
*  Standards Of Fundamental Astronomy Review Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING TERMS AND CONDITIONS
*  WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Review Board ("the Board").
*
*  2. The Software is made available free of charge for use by:
*
*     a) private individuals for non-profit research; and
*
*     b) non-profit educational, academic and research institutions.
*
*  3. Commercial use of the Software is specifically excluded from the
*     terms and conditions of this license.  Commercial use of the
*     Software is subject to the prior written agreement of the Board on
*     terms to be agreed.
*
*  4. The provision of any version of the Software under the terms and
*     conditions specified herein does not imply that future versions
*     will also be made available under the same terms and conditions.
*
*  5. The user may modify the Software for his/her own purposes.  The
*     user may distribute the modified software provided that the Board
*     is informed and that a copy of the modified software is made
*     available to the Board on request.  All modifications made by the
*     user shall be clearly identified to show how the modified software
*     differs from the original Software, and the name(s) of the
*     affected routine(s) shall be changed.  The original SOFA Software
*     License text must be present.
*
*  6. In any published work produced by the user and which includes
*     results achieved by using the Software, the user shall acknowledge
*     that the Software was used in producing the information contained
*     in such publication.
*
*  7. The user may incorporate or embed the Software into other software
*     products which he/she may then give away free of charge but not
*     sell provided the user makes due acknowledgement of the use which
*     he/she has made of the Software in creating such software
*     products.  Any redistribution of the Software in this way shall be
*     made under the same terms and conditions under which the user
*     received it from the SOFA Center.
*
*  8. The user shall not cause the Software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or by
*     inappropriate modification.
*
*  9. The Software is provided to the user "as is" and the Board makes
*     no warranty as to its use or performance.   The Board does not and
*     cannot warrant the performance or results which the user may
*     obtain by using the Software.  The Board makes no warranties,
*     express or implied, as to non-infringement of third party rights,
*     merchantability, or fitness for any particular purpose.  In no
*     event will the Board be liable to the user for any consequential,
*     incidental, or special damages, including any lost profits or lost
*     savings, even if a Board representative has been advised of such
*     damages, or for any claim by any third party.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*     Internet email: sofa@rl.ac.uk
*     Postal address: IAU SOFA Center
*                     Rutherford Appleton Laboratory
*                     Chilton, Didcot, Oxon OX11 0QX
*                     United Kingdom
*
*
*-----------------------------------------------------------------------

      END




c
c
c
c     Subroutine fov_0
c
c
c     Astrometry of images, only. (Astrometry option 0).
c
c
c
c
c     Extracts or estimates the (RA,Dec) limits and tangent point (the center
c     so to speak) of the FOV. Also extracts the instant of observation in JD
c     and Julian year, among other FOV auxiliary information.
c
c
c     rac = RA  center in degrees
c     dec = Dec center in degrees
c     
c     ramax = maximum RA in the FOV in degrees
c     ramin = minimum RA in the FOV in degrees
c
c     demax = maximum Dec in the FOV in degrees
c     demin = minimum Dec in the FOV in degrees
c
c
c
c     The FOV's (RA,Dec) limits may be expanded or contracted by a factor
c     given by the user. If the factor is 1 the limits are unchanged.
c
c
c 
c
c      Last modification: M. Assafin 13/Nov/2016
c
c
c
c


      subroutine fov_0 (centro,infits,expfov,nx,ny,scala,rac,dec,ramin,
     ?ramax,demin,demax,iah,iam,sa,isig,idg,idm,ds,iuth,iutm,sut,iutano,
     ?iutmes,iutdia,dj,epoj,iexps,ichfil,mchobj)




      implicit real *8 (a-h,o-z)


      character*50 centro
      character*69 mchobj
      character*20 ichfil
      character*1  isig
      character*150 infits,imfits


      hmsgms(i,j,a)=i+j/60.d0+a/3600.d0

      dexy(xx,yy,zz,ww)=dsin(yy)*dsin(ww)+dcos(yy)*dcos(ww)*dcos(xx-zz)
      xpad(xx,yy,zz)=dcos(yy)*dsin(xx-zz)
      ypad(xx,yy,zz,ww)=dsin(yy)*dcos(ww)-dcos(yy)*dsin(ww)*dcos(xx-zz)



c
c     Initial data
c
c

      pi=0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c

      dj2000=2451545.d0



c
c
c     Opens field file data with extracted information from fits images' headers
c     using the PRAIA_header_extraction task.
c
c

      open (77,file=centro)

c

      k=0

 400  continue

      imfits=''

      read(77,402,end=403) iah,iam,sa,isig,idg,idm,ds,iuth,iutm,sut,
     ?iutano,iutmes,iutdia,djm,dj,iexps,ichfil,imfits,mchobj

 402  format(1x,i2,1x,i2,1x,f7.4,1x,a1,i2,1x,i2,1x,f6.3,2x,i2,1x,i2,
     ?1x,f5.2,1x,i4,1x,i2,1x,i2,f16.8,1x,f16.8,2x,i4,2x,a20,2x,a50,
     ?1x,a20)



      if (imfits.ne.infits) go to 400

      k=1

 403  close (77)


c
c     No input data found
c


      if (k.ne.1) then
      write (*,*) 'No field data found. Exiting program'
      stop
      endif


c
c     Julian year of observation
c


      epoj=2000d0+(dj-dj2000)/365.25d0


c
c     FOV (RA,Dec) center in degrees
c


      rac=hmsgms(iah,iam,sa)*15.d0
      dec=hmsgms(idg,idm,ds)
      if (isig.eq.'-') dec=-dec




c
c     Estimated (x,y) field sizes in degrees
c


      dx=nx*scala/3600.d0
      dy=ny*scala/3600.d0


c
c     We don't know what is the (RA,Dec) association with
c     (x,y) due to possible field rotations.
c
c     So, picks up the largest value and associate this
c     size to both RA and Dec.
c


      dd=dmax1(dx,dy)


c
c     Applies the user's expansion/contraction factor
c


      dd=dd*expfov


c
c     Checks if pole (north or south) falls in the FOV
c
c
c     krcat = 1  -->  pole is not in the FOV 
c     krcat = 2  -->  pole is inside the FOV 
c
c


      krcat=1

      grac=grarad*rac
      gdec=grarad*dec

      bra=0.d0

      if (dec.lt.0.d0) then
      bde=-90.d0*grarad
      else      
      bde=+90.d0*grarad
      endif

      d=dexy(bra,bde,grac,gdec)
      xx=xpad(bra,bde,grac)/d
      yy=ypad(bra,bde,grac,gdec)/d

      xx=radgra*xx
      yy=radgra*yy


      if (dabs(xx).le.dd/2.d0 .and. dabs(yy).le.dd/2.d0) krcat=2


c
c     (RA,Dec) FOV limits for pole inside the FOV
c


      if (krcat.eq.2) then

      ramin=0.d0
      ramax=360.d0


      if (dec.ge.0.d0) then

      demax=+90.d0
      demin=dec-dd/2.d0
      if (demin.lt.-90.d0) demin=-90.d0

      else

      demin=-90.d0
      demax=dec+dd/2.d0
      if (demax.gt.+90.d0) demax=+90.d0

      endif

      return

      endif



c
c     (RA,Dec) FOV limits for pole not inside the FOV
c


      if (krcat.eq.1) then

      ramax=rac+dd/2.d0
      if (ramax.gt.360.d0) ramax=ramax-360.d0

      ramin=rac-dd/2.d0
      if (ramin.lt.0.d0) ramin=ramin+360.d0



      demax=dec+dd/2.d0
      if (demax.gt.+90.d0) demax=+90.d0

      demin=dec-dd/2.d0
      if (demin.lt.-90.d0) demin=-90.d0


      return

      endif

c

      return

      end




c
c
c
c     Subroutine fov_12
c
c
c     Remaking astrometry of xy PRAIA file, only (Astrometry options 1,2).
c
c     Applies to remaking astrometry based on both (x,y)s or (RA,Dec)s (tangent
c     plane technique). 
c
c
c     Extracts or estimates the (RA,Dec) limits and tangent point (the center
c     so to speak) of the FOV. 
c
c
c     xra = RA  of stars in degrees from former reduction
c     yde = Dec of stars in degrees from former reduction
c
c     rac = RA  center in degrees
c     dec = Dec center in degrees
c     
c     ramax = maximum RA in the FOV in degrees
c     ramin = minimum RA in the FOV in degrees
c
c     demax = maximum Dec in the FOV in degrees
c     demin = minimum Dec in the FOV in degrees
c
c
c
c     Here, the FOV's (RA,Dec) limits are not expanded or contracted by a factor
c     given by the user. The FOV's (RA,Dec) true limits are directly determined
c     from the stars' (RA,Dec)s obtained in the previous (RA,Dec) reduction.
c
c
c 
c
c      Last modification: M. Assafin 26/Jul/2017
c
c
c
c


      subroutine fov_12 (idiobs,nest,xra,yde,rac,dec,ramin,ramax,demin,
     ?demax,iah,iam,sa,isig,idg,idm,ds)


      implicit real *8 (a-h,o-z)


      dimension xra(idiobs),yde(idiobs)

      character*1 isig


      dexy(xx,yy,zz,ww)=dsin(yy)*dsin(ww)+dcos(yy)*dcos(ww)*dcos(xx-zz)
      xpad(xx,yy,zz)=dcos(yy)*dsin(xx-zz)
      ypad(xx,yy,zz,ww)=dsin(yy)*dcos(ww)-dcos(yy)*dsin(ww)*dcos(xx-zz)


      alff (xx,yy,zz,ww)=zz+datan2(xx,dcos(ww)-yy*dsin(ww))
      deltt(xx,yy,zz,ww)=datan2((yy*dcos(ww)+dsin(ww))*dcos(xx-zz),
     ?dcos(ww)-yy*dsin(ww))



c
c     Initial data
c
c

      pi=0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi


      safe=5.d0/3600.d0

c
c     Gets some parameters from the FOV stars
c


      ramin=361.d0
      ramax=-1.d0

      demin=+91.d0
      demax=-91.d0
      


      do j=1,nest

      if (yde(j).lt.demin) demin=yde(j)
      if (yde(j).gt.demax) demax=yde(j)

      if (xra(j).lt.ramin) ramin=xra(j)
      if (xra(j).gt.ramax) ramax=xra(j)


      enddo


c
c     Provisional (RA,Dec) FOV center
c


      ram=(ramax+ramin)/2.d0
      dem=(demax+demin)/2.d0



c
c     Detemines final (RA,Dec) FOV center:
c
c
c     1) gnomonic projection of all star's (RA,Dec)s w.r.t the
c        provivional center
c
c     2) computes (X,Y)'s baricenter
c
c     3) anti-gnomonic projection of (X,Y) baricenter to get
c        the (RA,Dec) baricenter of the FOV
c
c     4) Use this as the final (RA,Dec) FOV center 
c
c
c     Note: here, it is irrelevant if the pole is or is not inside 
c           the FOV
c 
c

      gram=grarad*ram
      gdem=grarad*dem


      x=0.d0
      y=0.d0

      do i=1,nest

      bra=xra(i)*grarad
      bde=yde(i)*grarad

      d=dexy(bra,bde,gram,gdem)
      xx=xpad(bra,bde,gram)/d
      yy=ypad(bra,bde,gram,gdem)/d

      x=x+xx
      y=y+yy

      enddo


      x=x/nest
      y=y/nest


      grac=alff(x,y,gram,gdem)
      gdec=deltt(grac,y,gram,gdem)


      rac=grac*radgra
      dec=gdec*radgra


    
c
c     (RA,Dec) of FOV in hexasegimal format
c 


      ra=rac/15.d0
      de=dec

      iah=ra
      am=(ra-iah)*60.d0
      iam=am
      sa =(am-iam)*60.d0
      if (de.lt.0.d0) then
      isig='-'
      de=-de
      else
      isig='+'
      endif
      idg=de
      dm=(de-idg)*60.d0
      idm=dm
      ds=(dm-idm)*60.d0



c
c     Adds a safe limit of 5 arcsec for finding stars in
c    (RA,Dec) extremes
c


      demax=demax+safe
      demin=demin-safe

      if (demax.gt.+90.d0) demax=+90.d0
      if (demin.lt.-90.d0) demin=-90.d0

c

      remax=remax+safe/dabs(dcos(grarad*dec))
      remin=remin-safe/dabs(dcos(grarad*dec))

      if (remax.gt.360.d0) remax=remax-360.d0
      if (remin.lt.0.d0) remin=remin+360.d0

c


      return

      end






c
c     LS fitting of function fpol/fpoly by Singular Value
c     Decomposition (Num. Recipies).
c
c     Last modified: M. Assafin  19/Jan/2019
c



      SUBROUTINE svdfit(x,y,z,sig,ndata,a,ma,u,v,w,mp,np,chisq,NMAX,
     ?MMAX,index)

      implicit real*8 (a-h,o-z)

c     dimension a(ma),sig(ndata),u(mp,np),v(np,np),w(np),x(ndata),
c    *y(ndata),z(ndata)

      dimension a(np),sig(ndata),u(mp,np),v(np,np),w(np),x(ndata),
     *y(ndata),z(ndata)

      dimension afunc(MMAX),b(NMAX)


c     INTEGER ma,mp,ndata,np,NMAX,MMAX
c     REAL chisq,a(ma),sig(ndata),u(mp,np),v(np,np),w(np),x(ndata),
c    *y(ndata),TOL
c     EXTERNAL funcs
c     PARAMETER (NMAX=1000,MMAX=50,TOL=1.e-5)
      PARAMETER (TOL=1.d-10)
CU    USES svbksb,svdcmp
c     INTEGER i,j
c     REAL sum,thresh,tmp,wmax,afunc(MMAX),b(NMAX)
      do 12 i=1,ndata
c       call funcs(x(i),afunc,ma)
        call funcs(mmax,index,x(i),y(i),afunc,ma,i,ndata)
        tmp=1.d0/sig(i)
        do 11 j=1,ma
          u(i,j)=afunc(j)*tmp
11      continue
c       b(i)=y(i)*tmp
        b(i)=z(i)*tmp
12    continue
      call svdcmp(u,ndata,ma,mp,np,w,v,NMAX)
      wmax=0.d0
      do 13 j=1,ma
        if(w(j).gt.wmax)wmax=w(j)
13    continue
      thresh=TOL*wmax
      do 14 j=1,ma
        if(w(j).lt.thresh)w(j)=0.d0
14    continue
      call svbksb(u,w,v,ndata,ma,mp,np,b,a,NMAX)
      chisq=0.d0
      do 16 i=1,ndata
c       call funcs(x(i),afunc,ma)
        call funcs(mmax,index,x(i),y(i),afunc,ma,i,ndata)
        sum=0.d0
        do 15 j=1,ma
          sum=sum+a(j)*afunc(j)
15      continue
c       chisq=chisq+((y(i)-sum)/sig(i))**2
        chisq=chisq+((z(i)-sum)/sig(i))**2
16    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ]2B,1.



      SUBROUTINE svdvar(v,ma,np,w,cvm,ncvm,MMAX)

      implicit real*8 (a-h,o-z)

      dimension cvm(ncvm,ncvm),v(np,np),w(np),wti(MMAX)

c     INTEGER ma,ncvm,np,MMAX
c     REAL cvm(ncvm,ncvm),v(np,np),w(np)
c     PARAMETER (MMAX=20)
c     INTEGER i,j,k
c     REAL sum,wti(MMAX)
      do 11 i=1,ma
        wti(i)=0.d0
        if(w(i).ne.0.d0) wti(i)=1.d0/(w(i)*w(i))
11    continue
      do 14 i=1,ma
        do 13 j=1,i
          sum=0.d0
          do 12 k=1,ma
            sum=sum+v(i,k)*v(j,k)*wti(k)
12        continue
          cvm(i,j)=sum
          cvm(j,i)=sum
13      continue
14    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ]2B,1.




      SUBROUTINE svdcmp(a,m,n,mp,np,w,v,NMAX)

      implicit real*8 (a-h,o-z)

      dimension a(mp,np),v(np,np),w(np),rv1(NMAX)

c     INTEGER m,mp,n,np,NMAX
c     REAL a(mp,np),v(np,np),w(np)
c     PARAMETER (NMAX=500)
CU    USES pythag
c     INTEGER i,its,j,jj,k,l,nm
c     REAL anorm,c,f,g,h,s,scale,x,y,z,rv1(NMAX),pythag
      g=0.0d0
      scale=0.0d0
      anorm=0.0d0
      do 25 i=1,n
        l=i+1
        rv1(i)=scale*g
        g=0.0d0
        s=0.0d0
        scale=0.0d0
        if(i.le.m)then
          do 11 k=i,m
            scale=scale+dabs(a(k,i))
11        continue
          if(scale.ne.0.0d0)then
            do 12 k=i,m
              a(k,i)=a(k,i)/scale
              s=s+a(k,i)*a(k,i)
12          continue
            f=a(i,i)
            g=-sign(dsqrt(s),f)
            h=f*g-s
            a(i,i)=f-g
            do 15 j=l,n
              s=0.0d0
              do 13 k=i,m
                s=s+a(k,i)*a(k,j)
13            continue
              f=s/h
              do 14 k=i,m
                a(k,j)=a(k,j)+f*a(k,i)
14            continue
15          continue
            do 16 k=i,m
              a(k,i)=scale*a(k,i)
16          continue
          endif
        endif
        w(i)=scale*g
        g=0.0d0
        s=0.0d0
        scale=0.0d0
        if((i.le.m).and.(i.ne.n))then
          do 17 k=l,n
            scale=scale+dabs(a(i,k))
17        continue
          if(scale.ne.0.0d0)then
            do 18 k=l,n
              a(i,k)=a(i,k)/scale
              s=s+a(i,k)*a(i,k)
18          continue
            f=a(i,l)
            g=-sign(dsqrt(s),f)
            h=f*g-s
            a(i,l)=f-g
            do 19 k=l,n
              rv1(k)=a(i,k)/h
19          continue
            do 23 j=l,m
              s=0.0d0
              do 21 k=l,n
                s=s+a(j,k)*a(i,k)
21            continue
              do 22 k=l,n
                a(j,k)=a(j,k)+s*rv1(k)
22            continue
23          continue
            do 24 k=l,n
              a(i,k)=scale*a(i,k)
24          continue
          endif
        endif
c       anorm=max1(anorm,(dabs(w(i))+dabs(rv1(i))))
        anorm=dmax1(anorm,(dabs(w(i))+dabs(rv1(i))))
25    continue
      do 32 i=n,1,-1
        if(i.lt.n)then
          if(g.ne.0.0d0)then
            do 26 j=l,n
              v(j,i)=(a(i,j)/a(i,l))/g
26          continue
            do 29 j=l,n
              s=0.0d0
              do 27 k=l,n
                s=s+a(i,k)*v(k,j)
27            continue
              do 28 k=l,n
                v(k,j)=v(k,j)+s*v(k,i)
28            continue
29          continue
          endif
          do 31 j=l,n
            v(i,j)=0.0d0
            v(j,i)=0.0d0
31        continue
        endif
        v(i,i)=1.0d0
        g=rv1(i)
        l=i
32    continue
c     do 39 i=min(m,n),1,-1
      do 39 i=min0(m,n),1,-1
        l=i+1
        g=w(i)
        do 33 j=l,n
          a(i,j)=0.0d0
33      continue
        if(g.ne.0.0d0)then
          g=1.0d0/g
          do 36 j=l,n
            s=0.0
            do 34 k=l,m
              s=s+a(k,i)*a(k,j)
34          continue
            f=(s/a(i,i))*g
            do 35 k=i,m
              a(k,j)=a(k,j)+f*a(k,i)
35          continue
36        continue
          do 37 j=i,m
            a(j,i)=a(j,i)*g
37        continue
        else
          do 38 j= i,m
            a(j,i)=0.0d0
38        continue
        endif
        a(i,i)=a(i,i)+1.0d0
39    continue
      do 49 k=n,1,-1
        do 48 its=1,30
          do 41 l=k,1,-1
            nm=l-1
            if((dabs(rv1(l))+anorm).eq.anorm)  goto 2
            if((dabs(w(nm))+anorm).eq.anorm)  goto 1
41        continue
1         c=0.0d0
          s=1.0d0
          do 43 i=l,k
            f=s*rv1(i)
            rv1(i)=c*rv1(i)
            if((dabs(f)+anorm).eq.anorm) goto 2
            g=w(i)
            h=pythag(f,g)
            w(i)=h
            h=1.0d0/h
            c= (g*h)
            s=-(f*h)
            do 42 j=1,m
              y=a(j,nm)
              z=a(j,i)
              a(j,nm)=(y*c)+(z*s)
              a(j,i)=-(y*s)+(z*c)
42          continue
43        continue
2         z=w(k)
          if(l.eq.k)then
            if(z.lt.0.0d0)then
              w(k)=-z
              do 44 j=1,n
                v(j,k)=-v(j,k)
44            continue
            endif
            goto 3
          endif
c         if(its.eq.30) then
          if(its.eq.600) then
          write (*,*)
          write (*,*) 'Warning: no convergence in subroutine svdcmp.'
          write (*,*)
c         stop
          return
          endif
          x=w(l)
          nm=k-1
          y=w(nm)
          g=rv1(nm)
          h=rv1(k)
          f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0d0*h*y)
          g=pythag(f,1.0d0)
          f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
          c=1.0d0
          s=1.0d0
          do 47 j=l,nm
            i=j+1
            g=rv1(i)
            y=w(i)
            h=s*g
            g=c*g
            z=pythag(f,h)
            rv1(j)=z
            c=f/z
            s=h/z
            f= (x*c)+(g*s)
            g=-(x*s)+(g*c)
            h=y*s
            y=y*c
            do 45 jj=1,n
              x=v(jj,j)
              z=v(jj,i)
              v(jj,j)= (x*c)+(z*s)
              v(jj,i)=-(x*s)+(z*c)
45          continue
            z=pythag(f,h)
            w(j)=z
            if(z.ne.0.0d0)then
              z=1.0d0/z
              c=f*z
              s=h*z
            endif
            f= (c*g)+(s*y)
            x=-(s*g)+(c*y)
            do 46 jj=1,m
              y=a(jj,j)
              z=a(jj,i)
              a(jj,j)= (y*c)+(z*s)
              a(jj,i)=-(y*s)+(z*c)
46          continue
47        continue
          rv1(l)=0.0d0
          rv1(k)=f
          w(k)=x
48      continue
3       continue
49    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ]2B,1.



      SUBROUTINE svbksb(u,w,v,m,n,mp,np,b,x,NMAX)

      implicit real*8 (a-h,o-z)

      dimension b(mp),u(mp,np),v(np,np),w(np),x(np),tmp(NMAX)

c     INTEGER m,mp,n,np,NMAX
c     REAL b(mp),u(mp,np),v(np,np),w(np),x(np)
c     PARAMETER (NMAX=500)
c     INTEGER i,j,jj
c     REAL s,tmp(NMAX)
      do 12 j=1,n
        s=0.d0
        if(w(j).ne.0.d0)then
          do 11 i=1,m
            s=s+u(i,j)*b(i)
11        continue
          s=s/w(j)
        endif
        tmp(j)=s
12    continue
      do 14 j=1,n
        s=0.d0
        do 13 jj=1,n
          s=s+v(j,jj)*tmp(jj)
13      continue
        x(j)=s
14    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ]2B,1.



      FUNCTION pythag(a,b)

      implicit real*8 (a-h,o-z)

c     REAL a,b,pythag
c     REAL absa,absb
      absa=dabs(a)
      absb=dabs(b)
      if(absa.gt.absb)then
        pythag=absa*dsqrt(1.d0+(absb/absa)**2)
      else
        if(absb.eq.0.d0)then
          pythag=0.d0
        else
          pythag=absb*dsqrt(1.d0+(absa/absb)**2)
        endif
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ]2B,1.



c
c     Subroutine funcs
c
c     Defines the terms of the function to be fit.
c
c
c     The function can be mono (x) or bivariate (x,y). It
c     is defined by the user, according to the following
c     indexes below:
c
c
c     Index = -1
c
c     Fitted function is a polynomial in x of degree np-1
c
c
c     Index = 0
c
c     Fitted function is a 4 cte model from (RA,Dec) reductions
c
c     X = 1.C + 0.D + ax + by  (first N/2 points)
c
c     Y = 0.C + 1.D + ay - bx  (last  N/2 points)
c
c     or equivalently a rotation of (x,y) axes.
c
c
c     Index = 1
c
c     Fitted function is a 1st degree polynomial
c     model from (RA,Dec) reductions.
c
c     (X or Y) = 1.c + ax + by
c
c
c     Index = 2
c
c     Fitted function is a complete 2nd degree
c     polynomial model from (RA,Dec) reductions.
c
c     (X or Y) = 1.f + ax + by + cx^2 + dxy + ey^2
c
c
c     Index = 3
c
c     Fitted function is a complete 3rd degree
c     polynomial model from (RA,Dec) reductions.
c
c     (X or Y) = 1.j + ax + by + cx^2 + dxy + ey^2 +
c                + fx^3 + gx^2y + hxy^2 + iy^3
c
c
c     Index = 41
c
c
c     Fitted function is a complete 2nd degree
c     polynomial model plus a X radial distortion
c     term of 3rd degree from (RA,Dec) reductions.
c
c     X = 1.g + ax + by + cx^2 + dxy + ey^2 +
c         + f x (x^2 + y^2)
c
c
c     Index = 42
c
c     Fitted function is a complete 2nd degree
c     polynomial model plus a Y radial distortion
c     term of 3rd degree from (RA,Dec) reductions.
c
c     Y = 1.g + ax + by + cx^2 + dxy + ey^2 +
c         + f y (x^2 + y^2)
c
c
c     Index = 5
c
c     Fitted function is a complete 5rd degree
c     polynomial model from (RA,Dec) reductions.
c
c     (X or Y) = 1.u + ax + by + cx^2 + dxy + ey^2 +
c                + fx^3 + gx^2y + hxy^2 + iy^3 +
c                + jx^4 + kx^3y + lx^2y^2 + mxy^3 + ny^4 +
c                + ox^5 + px^4y + qx^3y^2 + rx^2y^3 +
c                + sxy^4 + ty^5
c
c
c     Index = 61
c
c     Fitted function is a complete 2nd degree
c     polynomial model plus X radial distortion
c     terms of 3rd and 5th degrees from
c     (RA,Dec) reductions.
c
c     X = 1.h + ax + by + cx^2 + dxy + ey^2 +
c         + f x (x^2 + y^2) + g x (x^2 + y^2)**2
c
c
c     Index = 62
c
c     Fitted function is a complete 2nd degree
c     polynomial model plus Y radial distortion
c     terms of 3rd and 5th degrees from
c     (RA,Dec) reductions.
c
c     Y = 1.h + ax + by + cx^2 + dxy + ey^2 +
c         + f y (x^2 + y^2) + g y (x^2 + y^2)**2
c
c
c     Index = 71
c
c     Fitted function is a complete 3rd degree
c     polynomial model plus a X radial distortion term
c     of the 5th order from (RA,Dec) reductions.
c
c     X = 1.k + ax + by + cx^2 + dxy + ey^2 +
c         + fx^3 + gx^2y + hxy^2 + iy^3 +
c         + j x (x^2 + y^2)^2
c
c
c     Index = 72
c
c     Fitted function is a complete 3rd degree
c     polynomial model plus a Y radial distortion term
c     of the 5th order from (RA,Dec) reductions.
c
c     Y = 1.k + ax + by + cx^2 + dxy + ey^2 +
c         + fx^3 + gx^2y + hxy^2 + iy^3 +
c         + j y (x^2 + y^2)^2
c
c
c
c
c
c     Variables:
c
c     id      - fixed dimension of vector p
c     index   - index indicating the function to be fit
c     x       - x value of the function
c     y       - y value of the function
c     p       - vector containing each of the terms of the
c               function which are multiplied by the
c               respective coefficients
c     np      - number of terms of the function (used for index=-1)
c     in      - nth point being evaluated (used for index=0)
c     nptos   - total number of points (used for index=0) 
c
c
c     Last modification: M. Assafin 17/Jan/2019
c
c

      SUBROUTINE funcs (id,index,x,y,p,np,in,nptos)

      implicit real*8 (a-h,o-z)

      dimension p(id)

c     dimension p(np)
c     INTEGER np
c     REAL x,p(np)
c     INTEGER j



c
c     Fitted function is a polynomial in x
c     of degree np-1
c
c     index = -1
c

      IF (index.eq.-1) THEN

      p(1)=1.d0
      do 11 j=2,np
        p(j)=p(j-1)*x
11    continue

      return

      ENDIF




c
c     Fitted function is a 4 cte model
c     from (RA,Dec) reductions
c
c     X = 1.C + 0.D + ax + by  (first N/2 points)
c
c     Y = 0.C + 1.D + ay - bx  (last  N/2 points)
c
c     or equivalently a rotation of (x,y) axes.
c
c
c     index = 0
c


      IF (index.eq.0) THEN

      j=nptos/2+0.1

      if (in.le.j) then

      p(1)=1.d0
      p(2)=0.d0
      p(3)=x
      p(4)=y

      else

      p(1)=0.d0
      p(2)=1.d0
      p(3)=y
      p(4)=-x

      endif

      return

      ENDIF




c
c     Fitted function is a 1st degree polynomial
c     model from (RA,Dec) reductions.
c
c     (X or Y) = 1.c + ax + by
c
c     index = 1
c

      IF (index.eq.1) THEN


      ngrau=1

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      p(icont)=(x**k)*(y**(j-1))

      enddo
      enddo

      return

      ENDIF



c
c     Fitted function is a complete 2nd degree
c     polynomial model from (RA,Dec) reductions.
c
c     (X or Y) = 1.f + ax + by + cx^2 + dxy + ey^2
c
c     index = 2
c

      IF (index.eq.2) THEN


      ngrau=2

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      p(icont)=(x**k)*(y**(j-1))

      enddo
      enddo

      return

      ENDIF



c
c     Fitted function is a complete 3rd degree
c     polynomial model from (RA,Dec) reductions.
c
c     (X or Y) = 1.j + ax + by + cx^2 + dxy + ey^2 +
c                + fx^3 + gx^2y + hxy^2 + iy^3
c
c     index = 3
c

      IF (index.eq.3) THEN


      ngrau=3

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      p(icont)=(x**k)*(y**(j-1))

      enddo
      enddo

      return

      ENDIF



c
c     Fitted function is a complete 2nd degree
c     polynomial model plus a X radial distortion
c     term of 3rd degree from (RA,Dec) reductions.
c
c      X = 1.g + ax + by + cx^2 + dxy + ey^2 +
c          + f x (x^2 + y^2)
c
c     index = 41
c

      IF (index.eq.41) THEN


      ngrau=2

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      p(icont)=(x**k)*(y**(j-1))

      enddo
      enddo

      p(icont+1)=x*(x**2+y**2)


      return

      ENDIF


c
c     Fitted function is a complete 2nd degree
c     polynomial model plus a Y radial distortion
c     term of 3rd degree from (RA,Dec) reductions.
c
c      Y = 1.g + ax + by + cx^2 + dxy + ey^2 +
c          + f y (x^2 + y^2)
c
c     index = 42
c

      IF (index.eq.42) THEN


      ngrau=2

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      p(icont)=(x**k)*(y**(j-1))

      enddo
      enddo

      p(icont+1)=y*(x**2+y**2)


      return

      ENDIF



c
c     Fitted function is a complete 5rd degree
c     polynomial model from (RA,Dec) reductions.
c
c     (X or Y) = 1.u + ax + by + cx^2 + dxy + ey^2 +
c                + fx^3 + gx^2y + hxy^2 + iy^3 +
c                + jx^4 + kx^3y + lx^2y^2 + mxy^3 + ny^4 +
c                + ox^5 + px^4y + qx^3y^2 + rx^2y^3 +
c                + sxy^4 + ty^5
c
c
c     index = 5
c

      IF (index.eq.5) THEN


      ngrau=5

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      p(icont)=(x**k)*(y**(j-1))

      enddo
      enddo

      return

      ENDIF



c
c     Fitted function is a complete 2nd degree
c     polynomial model plus X radial distortion
c     terms of 3rd and 5th degrees from (RA,Dec)
c     reductions.
c
c      X = 1.h + ax + by + cx^2 + dxy + ey^2 +
c          + f x (x^2 + y^2) + g x (x^2 + y^2)**2
c
c     index = 61
c

      IF (index.eq.61) THEN


      ngrau=2

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      p(icont)=(x**k)*(y**(j-1))

      enddo
      enddo

      p(icont+1)=x*(x**2+y**2)

      p(icont+2)=x*(x**2+y**2)**2

      return

      ENDIF


c
c     Fitted function is a complete 2nd degree
c     polynomial model plus Y radial distortion
c     terms of 3rd and 5th degrees from (RA,Dec)
c     reductions.
c
c      Y = 1.g + ax + by + cx^2 + dxy + ey^2 +
c         + f y (x^2 + y^2) + g y (x^2 + y^2)**2
c
c     index = 62
c

      IF (index.eq.62) THEN


      ngrau=2

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      p(icont)=(x**k)*(y**(j-1))

      enddo
      enddo

      p(icont+1)=y*(x**2+y**2)

      p(icont+2)=y*(x**2+y**2)**2

      return

      ENDIF




c
c     Fitted function is a complete 3rd degree
c     polynomial model plus a X radial distortion term
c     of the fifth order from (RA,Dec) reductions.
c
c      X = 1.k + ax + by + cx^2 + dxy + ey^2 +
c          + fx^3 + gx^2y + hxy^2 + iy^3 +
c          + j x (x^2 + y^2)^2
c
c
c     index = 71
c

      IF (index.eq.71) THEN


      ngrau=3

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      p(icont)=(x**k)*(y**(j-1))

      enddo
      enddo

      p(icont+1)=x*(x**2+y**2)**2


      return

      ENDIF




c
c     Fitted function is a complete 3rd degree
c     polynomial model plus a Y radial distortion term
c     of the fifth order from (RA,Dec) reductions.
c
c      Y = 1.k + ax + by + cx^2 + dxy + ey^2 +
c          + fx^3 + gx^2y + hxy^2 + iy^3 +
c          + j y (x^2 + y^2)^2
c
c
c     index = 72
c

      IF (index.eq.72) THEN


      ngrau=3

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      p(icont)=(x**k)*(y**(j-1))

      enddo
      enddo

      p(icont+1)=y*(x**2+y**2)**2


      return

      ENDIF


c

      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ]2B,1.






c
c     Function fpol
c
c     Gives the value of the function from the input
c     variables and function coefficients.
c
c
c     The function can be mono (x) or bivariate (x,y). It
c     is defined by the user, according to the following
c     indexes below:
c
c
c     Index = -1
c
c     Function is a polynomial in x of degree np-1
c
c
c     Index = 0
c
c     Function is a 4 cte model from (RA,Dec) reductions
c
c     X = 1.C + 0.D + ax + by  (first N/2 points)
c
c     Y = 0.C + 1.D + ay - bx  (last  N/2 points)
c
c     or equivalently a rotation of (x,y) axes.
c
c
c     Index = 1
c
c     Function is a 1st degree polynomial
c     model from (RA,Dec) reductions.
c
c     (X or Y) = 1.c + ax + by
c
c
c     Index = 2
c
c     Function is a complete 2nd degree
c     polynomial model from (RA,Dec) reductions.
c
c     (X or Y) = 1.f + ax + by + cx^2 + dxy + ey^2
c
c
c     Index = 3
c
c     Function is a complete 3rd degree
c     polynomial model from (RA,Dec) reductions.
c
c     (X or Y) = 1.j + ax + by + cx^2 + dxy + ey^2 +
c                + fx^3 + gx^2y + hxy^2 + iy^3
c
c
c     Index = 41
c
c
c     Function is a complete 2nd degree
c     polynomial model plus a X radial distortion
c     term of 3rd degree from (RA,Dec) reductions.
c
c     X = 1.g + ax + by + cx^2 + dxy + ey^2 +
c         + f x (x^2 + y^2)
c
c
c     Index = 42
c
c     Function is a complete 2nd degree
c     polynomial model plus a Y radial distortion
c     term of 3rd degree from (RA,Dec) reductions.
c
c     Y = 1.g + ax + by + cx^2 + dxy + ey^2 +
c         + f y (x^2 + y^2)
c
c
c     Index = 5
c
c     Function is a complete 5rd degree
c     polynomial model from (RA,Dec) reductions.
c
c     (X or Y) = 1.u + ax + by + cx^2 + dxy + ey^2 +
c                + fx^3 + gx^2y + hxy^2 + iy^3 +
c                + jx^4 + kx^3y + lx^2y^2 + mxy^3 + ny^4 +
c                + ox^5 + px^4y + qx^3y^2 + rx^2y^3 +
c                + sxy^4 + ty^5
c
c
c     Index = 61
c
c     Function is a complete 2nd degree
c     polynomial model plus X radial distortion
c     terms of 3rd and 5th degrees from
c     (RA,Dec) reductions.
c
c     X = 1.h + ax + by + cx^2 + dxy + ey^2 +
c         + f x (x^2 + y^2) + g x (x^2 + y^2)**2
c
c
c     Index = 62
c
c     Function is a complete 2nd degree
c     polynomial model plus Y radial distortion
c     terms of 3rd and 5th degrees from
c     (RA,Dec) reductions.
c
c     Y = 1.h + ax + by + cx^2 + dxy + ey^2 +
c         + f y (x^2 + y^2) + g y (x^2 + y^2)**2
c
c
c     Index = 71
c
c     Function is a complete 3rd degree
c     polynomial model plus a X radial distortion term
c     of the 5th order from (RA,Dec) reductions.
c
c     X = 1.k + ax + by + cx^2 + dxy + ey^2 +
c         + fx^3 + gx^2y + hxy^2 + iy^3 +
c         + j x (x^2 + y^2)^2
c
c
c     Index = 72
c
c     Function is a complete 3rd degree
c     polynomial model plus a Y radial distortion term
c     of the 5th order from (RA,Dec) reductions.
c
c     Y = 1.k + ax + by + cx^2 + dxy + ey^2 +
c         + fx^3 + gx^2y + hxy^2 + iy^3 +
c         + j y (x^2 + y^2)^2
c
c
c
c     Variables:
c
c     id      - fixed dimension of vector p
c     index   - index indicating the function to be fit
c     x       - x value of the function
c     y       - y value of the function
c     p       - vector containing each of the terms of the
c               function which are multiplied by the
c               respective coefficients
c     np      - number of terms of the function (used for index = -1)
c     in      - nth point being evaluated (used for index=0)
c     nptos   - total number of points (used for index=0) 
c
c
c
c     Last modification: M. Assafin 17/Jan/2019
c
c


      double precision function fpol (id,index,x,y,coef,np,in,nptos)

      implicit real*8 (a-h,o-z)

      dimension coef(id)

      fpol=0.d0



c
c     Function is a polynomial in x
c     of degree np-1
c
c     index = -1
c

      IF (index.eq.-1) THEN

      do i=1,np
      fpol=fpol+coef(i)*x**(i-1)
      enddo

      return

      ENDIF



c
c     Function is a 4 cte model
c     from (RA,Dec) reductions
c
c     X = 1.C + 0.D + ax + by  (first N/2 points)
c
c     Y = 0.C + 1.D + ay - bx  (last  N/2 points)
c
c     or equivalently a rotation of (x,y) axes.
c
c
c     index = 0
c


      IF (index.eq.0) THEN

      j=nptos/2+0.1

      if (in.le.j) then

      fpol=coef(1)+x*coef(3)+y*coef(4)

      else

      fpol=coef(2)+y*coef(3)-x*coef(4)

      endif

      return

      ENDIF




c
c     Function is a 1st degree polynomial
c     model from (RA,Dec) reductions.
c
c     (X or Y) = 1.c + ax + by
c
c     index = 1
c

      IF (index.eq.1) THEN


      ngrau=1

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      fpol=fpol+coef(icont)*(x**k)*(y**(j-1))

      enddo
      enddo

      return

      ENDIF




c
c     Function is a complete 2nd degree
c     polynomial model from (RA,Dec) reductions.
c
c     (X or Y) = 1.f + ax + by + cx^2 + dxy + ey^2
c
c     index = 2
c

      IF (index.eq.2) THEN


      ngrau=2

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      fpol=fpol+coef(icont)*(x**k)*(y**(j-1))

      enddo
      enddo

      return

      ENDIF



c
c     Function is a complete 3rd degree
c     polynomial model from (RA,Dec) reductions.
c
c     (X or Y) = 1.j + ax + by + cx^2 + dxy + ey^2 +
c                + fx^3 + gx^2y + hxy^2 + iy^3
c
c     index = 3
c

      IF (index.eq.3) THEN


      ngrau=3

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      fpol=fpol+coef(icont)*(x**k)*(y**(j-1))

      enddo
      enddo

      return

      ENDIF




c
c     Function is a complete 2nd degree
c     polynomial model plus a X radial distortion
c     term of 3rd degree from (RA,Dec) reductions.
c
c      X = 1.g + ax + by + cx^2 + dxy + ey^2 +
c          + f x (x^2 + y^2)
c
c     index = 41
c

      IF (index.eq.41) THEN


      ngrau=2

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      fpol=fpol+coef(icont)*(x**k)*(y**(j-1))

      enddo
      enddo

      fpol=fpol+coef(icont+1)*x*(x**2+y**2)

      return

      ENDIF


c
c     Function is a complete 2nd degree
c     polynomial model plus a Y radial distortion
c     term of 3rd degree from (RA,Dec) reductions.
c
c      Y = 1.g + ax + by + cx^2 + dxy + ey^2 +
c          + f y (x^2 + y^2)
c
c     index = 42
c

      IF (index.eq.42) THEN


      ngrau=2

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      fpol=fpol+coef(icont)*(x**k)*(y**(j-1))

      enddo
      enddo

      fpol=fpol+coef(icont+1)*y*(x**2+y**2)

      return

      ENDIF



c
c     Fitted function is a complete 5rd degree
c     polynomial model from (RA,Dec) reductions.
c
c     (X or Y) = 1.u + ax + by + cx^2 + dxy + ey^2 +
c                + fx^3 + gx^2y + hxy^2 + iy^3 +
c                + jx^4 + kx^3y + lx^2y^2 + mxy^3 + ny^4 +
c                + ox^5 + px^4y + qx^3y^2 + rx^2y^3 +
c                + sxy^4 + ty^5
c
c
c     index = 5
c

      IF (index.eq.5) THEN


      ngrau=5

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      fpol=fpol+coef(icont)*(x**k)*(y**(j-1))

      enddo
      enddo

      return

      ENDIF


c
c     Function is a complete 2nd degree
c     polynomial model plus X radial distortion
c     terms of 3rd and 5th degrees from
c     (RA,Dec) reductions.
c
c     X = 1.h + ax + by + cx^2 + dxy + ey^2 +
c         + f x (x^2 + y^2) + g x (x^2 + y^2)**2
c
c     index = 61
c

      IF (index.eq.61) THEN


      ngrau=2

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      fpol=fpol+coef(icont)*(x**k)*(y**(j-1))

      enddo
      enddo

      fpol=fpol+coef(icont+1)*x*(x**2+y**2)

      fpol=fpol+coef(icont+2)*x*(x**2+y**2)**2

      return

      ENDIF


c
c     Function is a complete 2nd degree
c     polynomial model plus Y radial distortion
c     terms of 3rd and 5th degrees from
c     (RA,Dec) reductions.
c
c     Y = 1.h + ax + by + cx^2 + dxy + ey^2 +
c         + f y (x^2 + y^2) + g y (x^2 + y^2)**2
c
c     index = 62
c

      IF (index.eq.62) THEN


      ngrau=2

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      fpol=fpol+coef(icont)*(x**k)*(y**(j-1))

      enddo
      enddo

      fpol=fpol+coef(icont+1)*y*(x**2+y**2)

      fpol=fpol+coef(icont+2)*y*(x**2+y**2)**2

      return

      ENDIF



c
c     Fitted function is a complete 3rd degree
c     polynomial model plus a X radial distortion term
c     of the fifth order from (RA,Dec) reductions.
c
c      X = 1.k + ax + by + cx^2 + dxy + ey^2 +
c          + fx^3 + gx^2y + hxy^2 + iy^3 +
c          + j x (x^2 + y^2)^2
c
c
c     index = 71
c

      IF (index.eq.71) THEN


      ngrau=3

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      fpol=fpol+coef(icont)*(x**k)*(y**(j-1))

      enddo
      enddo

      fpol=fpol+coef(icont+1)*x*(x**2+y**2)**2

      return

      ENDIF




c
c     Fitted function is a complete 3rd degree
c     polynomial model plus a Y radial distortion term
c     of the fifth order from (RA,Dec) reductions.
c
c      Y = 1.k + ax + by + cx^2 + dxy + ey^2 +
c          + fx^3 + gx^2y + hxy^2 + iy^3 +
c          + j y (x^2 + y^2)^2
c
c
c     index = 72
c

      IF (index.eq.72) THEN


      ngrau=3

      icont=0
      do i=1,ngrau+1
      do j=1,i

      k=i-j
      icont=icont+1

      fpol=fpol+coef(icont)*(x**k)*(y**(j-1))

      enddo
      enddo

      fpol=fpol+coef(icont+1)*y*(x**2+y**2)**2

      return

      ENDIF

c


      return
      end



c
c     Subroutine ids9
c
c
c
c     Outputs ds9 region file for each reference
c     catalogue reduction. 
c
c
c     Regions can be circles (rounded objects) or
c     rectangles (trace-shaped objects).
c
c
c     Color index:
c
c
c     - black : reference object not identified in the FOV;
c
c     - orange: catalogue object filtered out (not used) prior to
c               the (RA,Dec) reduction (mag. range, proper motion,
c               parallax or duplicity cutoff);
c
c     - green : reference object used in (RA,Dec) reduction;
c
c     - yellow: reference object eliminated in (RA,Dec) reduction;
c
c     - red   : target in the FOV;
c
c     - blue  : not identified object (not a reference catalogue
c               object or target).
c
c
c
c     Objects drawn with double circles were detected by force after
c     preliminary (RA,Dec) reduction.
c
c
c
c
c     Last modification: M. Assafin   19/Jan/2019
c
c


      subroutine ids9 (idiobs,linxy,kds9,ixy,obtype,xob,yob,cra,ilado,
     ?angle,nest,n)

      implicit real*8 (a-h,o-z)

      dimension xob(idiobs),yob(idiobs),cra(idiobs),ilado(idiobs),
     ?angle(idiobs)

      character*(linxy) ixy(idiobs),linha

      character*50 kds9

      character*56 ifor01,ifor02,ifor03,ifor04,ifor05,ifor06,kfor01,
     ?kfor02,kfor03,kfor04,kfor05

      character*60 mfor,lfor


      character*1 obtype(idiobs)

      character*5 kind


c
c     Initial variables
c


      pi=0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c
c     Formats
c


      ifor01="('image; circle(',2(f16.8,','),f16.8,') # color=green ')"
      ifor02="('image; circle(',2(f16.8,','),f16.8,') # color=yellow')"
      ifor03="('image; circle(',2(f16.8,','),f16.8,') # color=red   ')"
      ifor04="('image; circle(',2(f16.8,','),f16.8,') # color=blue  ')"
      ifor05="('image; circle(',2(f16.8,','),f16.8,') # color=orange')"

      ifor06="('image; circle(',2(f16.8,','),f16.8,') # color=black ')"

      kfor01="('image; box(',4(f16.8,','),f16.8,') # color=green    ')"
      kfor02="('image; box(',4(f16.8,','),f16.8,') # color=yellow   ')"
      kfor03="('image; box(',4(f16.8,','),f16.8,') # color=red      ')"
      kfor04="('image; box(',4(f16.8,','),f16.8,') # color=blue     ')"
      kfor05="('image; box(',4(f16.8,','),f16.8,') # color=orange   ')"

      mfor="('# text(',f16.8,',',f16.8,') text={',i4.4,'} color=orange')
     ?"

      lfor="('# text(',f16.8,',',f16.8,') text={',i4.4,'} color=black ')
     ?"

c

      open(91,file=kds9)

      DO i=1,nest

      linha=''
      linha=ixy(i)

      read(linha(233:236),*) icode

      kind=''

      read(linha(419:423),'(a5)') kind


      IF (obtype(i).eq.'r') THEN

c
c     Round-shaped objects
c



      xlado=cra(i)

      if (icode.eq.0) write(91,ifor01) xob(i),yob(i),xlado
      if (icode.eq.1) write(91,ifor02) xob(i),yob(i),xlado
      if (icode.eq.7) write(91,ifor03) xob(i),yob(i),xlado
      if (icode.eq.8) write(91,ifor03) xob(i),yob(i),xlado
      if (icode.eq.99) write(91,ifor04) xob(i),yob(i),xlado
 
      if ((icode.gt.10.and.icode.ne.99).or.icode.eq.3) then

      write(91,ifor05) xob(i),yob(i),xlado 

      if (kind.ne.'BOIA2'.and.kind.ne.'BOIA3') write(91,mfor) xob(i),
     ?yob(i)+2.0d0*xlado,icode

      endif

c
c     Draws double circles for primary-detected objects with final
c     (x,y) measurements from post-detection
c

      if (kind.eq.'BOIA2') then

      if (icode.eq.0) write(91,ifor01) xob(i),yob(i),xlado+5.d0
      if (icode.eq.1) write(91,ifor02) xob(i),yob(i),xlado+5.d0
      if (icode.eq.7) write(91,ifor03) xob(i),yob(i),xlado+5.d0
      if (icode.eq.8) write(91,ifor03) xob(i),yob(i),xlado+5.d0
      if (icode.eq.99) write(91,ifor04) xob(i),yob(i),xlado+5.d0


      if ((icode.gt.10.and.icode.ne.99).or.icode.eq.3) then

      write(91,ifor05) xob(i),yob(i),xlado+5.d0

      write(91,mfor) xob(i),yob(i)+2.0d0*xlado+5.d0,icode

      endif

      endif

c
c     Draws triple circles for objects with (x,y) measurements
c     only from post-detection
c


      if (kind.eq.'BOIA3') then

      if (icode.eq.0) write(91,ifor01) xob(i),yob(i),xlado+5.d0
      if (icode.eq.1) write(91,ifor02) xob(i),yob(i),xlado+5.d0
      if (icode.eq.7) write(91,ifor03) xob(i),yob(i),xlado+5.d0
      if (icode.eq.8) write(91,ifor03) xob(i),yob(i),xlado+5.d0
      if (icode.eq.99) write(91,ifor04) xob(i),yob(i),xlado+5.d0

      if (icode.eq.0) write(91,ifor01) xob(i),yob(i),xlado+10.d0
      if (icode.eq.1) write(91,ifor02) xob(i),yob(i),xlado+10.d0
      if (icode.eq.7) write(91,ifor03) xob(i),yob(i),xlado+10.d0
      if (icode.eq.8) write(91,ifor03) xob(i),yob(i),xlado+10.d0
      if (icode.eq.99) write(91,ifor04) xob(i),yob(i),xlado+10.d0


      if ((icode.gt.10.and.icode.ne.99).or.icode.eq.3) then

      write(91,ifor05) xob(i),yob(i),xlado+5.d0

      write(91,ifor05) xob(i),yob(i),xlado+10.d0

      write(91,mfor) xob(i),yob(i)+2.0d0*xlado+10.d0,icode

      endif

      endif








      ELSE


c
c     Trace-shaped objects
c

      xlado=ilado(i)
      ylado=3.d0*cra(i)
      tetha=angle(i)*radgra

      if (icode.eq.0) write(91,kfor01) xob(i),yob(i),xlado,ylado,tetha
      if (icode.eq.1) write(91,kfor02) xob(i),yob(i),xlado,ylado,tetha
      if (icode.eq.7) write(91,kfor03) xob(i),yob(i),xlado,ylado,tetha
      if (icode.eq.8) write(91,kfor03) xob(i),yob(i),xlado,ylado,tetha
      if (icode.eq.99) write(91,kfor04) xob(i),yob(i),xlado,ylado,tetha

      if ((icode.gt.10.and.icode.ne.99).or.icode.eq.3) then

      write(91,kfor05) xob(i),yob(i),xlado,ylado,tetha

      write(91,mfor) xob(i),yob(i)+2.0d0*ylado,icode

      endif 


      ENDIF


      ENDDO


c
c     Reference catalogue objects not identified in the FOV
c


      DO i=nest+1,nest+n

      linha=''
      linha=ixy(i)

      read(linha(233:236),*) icode

      xlado=2.5d0*cra(i)

      write(91,ifor06) xob(i),yob(i),xlado
 
      write(91,lfor) xob(i),yob(i)+2.0d0*xlado,icode


      ENDDO


c

      close (91)

      return

      end


*----------------------------------------------------------------------------------
*
*  Copyright (c) 2023 PRAIA Marcelo Assafin
*
*  ======================
*  PRAIA Software License
*  ======================
*
*  Permission is hereby granted, free of charge, to any person obtaining a copy
*  of this software and associated documentation files (the “Software”), to deal
*  in the Software without restriction, including without limitation the rights
*  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
*  of the Software, and to permit persons to whom the Software is furnished to do
*  so, subject to the following conditions:
*      
*  Note:
*
*  1. THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
*  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
*  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
*  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*
*  2. SOFA SOFTWARE USED BY PRAIA IS UNDER SOFA SOFTWARE LICENSE
*
*  3. NUMERICAL RECIPES SOFTWARE USED BY PRAIA IS UNDER NUMERICAL RECIPES SOFTWARE
*  LICENSE
*
*  Correspondence concerning PRAIA software should be addressed as follows:
*
*     Internet email: massaf@ov.ufrj.br   (insert the word "PRAIA" in the subject)
*     Postal address: Marcelo Assafin - PRAIA
*                     Observatorio do Valongo
*                     Ladeira do Pedro Antonio 43 - Saude
*                     20.080-090 Rio de Janeiro, RJ, Brazil
*
*----------------------------------------------------------------------------------
      

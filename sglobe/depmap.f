      character*100 getunx,dummy,hfmt,param,hmodel,ofile



      call chekcl('|   :r:1:Name of input model'
     1          //'|-f :o:1:Model format e.g. pct, sph, gml. Defaults to'
     1                                //' last 3 characters of file name'
     1          //'|-c :o:3:lmin,lmax,ifeven'
     1          //'|-p :o:1:[vs] Parameter'
     1          //'|-d :r:1,*:Depths'
     1          //'|')
      dummy=getunx('-c',1,ll)
      if(ll.gt.0) then
        read(dummy(1:ll),*) lminc
        dummy=getunx('-c',2,ll)
        read(dummy(1:ll),*) lmaxc
        dummy=getunx('-c',3,ll)
        read(dummy(1:ll),*) ifevenc
      else
        lminc=0
        lmaxc=9999
        ifevenc=0
      endif
      param=getunx('-p',1,lparam)

      call splksetup()
      call splhsetup()
         

      hmodel=getunx(' ',1,lhmodel)
      ip1=lhmodel
      do while(ip1.gt.0.and.hmodel(ip1:ip1).ne.'/')
        ip1=ip1-1
      enddo
    
      hfmt=getunx('-f',1,lhfmt)
      if(lhfmt.le.0) then
        hfmt=hmodel(lhmodel-2:lhmodel)
        lhfmt=3
      endif
      write(6,*) 'hmodel=('//hmodel(1:lhmodel)//') lhmodel=',lhmodel
      write(6,*) 'hfmt=('//hfmt(1:lhfmt)//') lhfmt=',lhfmt
  100 idep=idep+1
      depth=reunx('-d',idep,ldep)
      if(ldep.le.0) stop
      write(ofile,'(a,''.D'',i4.4,''.raw'')') hmodel(ip1+1:lhmodel-4),ifix(depth)
      lofile=istlen(ofile)
      call mapit00(hmodel(1:lhmodel)
     1   ,hfmt(1:lhfmt)
     1   ,param(1:lparam)
     1   ,depth,i1,j2,iwidth,iheight,z,ifa
     1    ,lminc,lmaxc,ifevenc,lmaxca,ifhisto,lloa,lhia,ofile(1:lofile))


      goto 100
      end

c----------------------------------------------------------------------
      subroutine mapit00(hmodel,hfmt,param,depth,i1i,j2i,iw,ih,z,ifa
     1     ,lminc,lmaxc,ifevenc,lmaxca,ifhisto,lloa,lhia,ofile)
      character*(*) hmodel,hfmt,param,ofile
      parameter (MXLYCF=50)
      dimension ycofs((MXLYCF+1)**2)
      parameter (MXLA=20)
      parameter (MXLENV=(MXLA-1)*(2*MXLA+6))
      dimension yani(MXLENV)
      call spharmap(hmodel,hfmt,param,depth,ycofs,lhigh,yani,MXLENV,lmaxa)
      if(lmaxc.ne.9999) then
        k=0
        do l=0,lhigh
          ifz=0
          if(l.lt.lminc.or.l.gt.lmaxc.or.(mod(l,2).ne.0.and.ifevenc.ne.0)) ifz=1
          do m=0,l
            k=k+1
            k1=k+1
            if(m.eq.0) then
              if(ifz.ne.0) ycofs(k)=0.
            else
              if(ifz.ne.0) ycofs(k)=0.
              if(ifz.ne.0) ycofs(k1)=0.
              k=k1
            endif
          enddo
        enddo
      endif
      open(66,file=ofile)
      write(66,'(i3)') lhigh
      write(66,'(5e16.8)') (ycofs(i),i=1,(lhigh+1)**2)
      close(66)
      end

c---------------------------------------------------------------------------

      subroutine spharmap(hmodeli,hfmt,param,depth,ycofs,lhigh,yani,mxyani,lmaxa)
      character*(*) hmodeli,hfmt,param
      character*80 hmodel
      dimension ycofs(*),yani(*)
      parameter (MXPARM=30)
      parameter (MXLORD=20)
      parameter (MXLENY=(MXLORD+1)**2)
      parameter (MXMDLL=MXLENY*MXPARM)
      common/hetmdl/ifani,lmaxum,lmaxum1,lenyu,npmm,pertmu(588),bmdlu(588)
      common/lmmdl/lmaxlm,lmaxlm1,lenyl,npar,pertml(588),bmdll(588)
      common/nmdl/lmaxn,nstr,nlp,anmdl(MXMDLL),bnmdl(MXMDLL)
      dimension mask(MXPARM),mask1(MXPARM),lask(0:MXLORD),lask1(0:MXLORD)
      common/mmdl/cmod(15,222,3),cmob(15,4),ycofs0(100),ycofs1(100)
      common/dvce/dvcof(121),divco(121),ifproj
      common/cmbc/lmcmb,cmbcof(121),cmbrot(121)
      common/gcof/lmgeoid,geocof(1369),gcofr(1369)
      parameter (MXLRAW=50)
      common/rawcof/lmraw,rawmdl( (MXLRAW+1)**2 )
      dimension vsc(50)

      lmaxa=0


      call complnam(hmodeli,hmodel,lhmodel)
      if(hfmt.eq.'div') then
        call rddivc(1,hmodel,10,dvcof)
        do i=1,121
          dvcof(i)=dvcof(i)*1.e+09
        enddo
        lmdiv=10

      else if(hfmt.eq.'geo') then
        call rgeoid(1,hmodel,lmgeoid,geocof)

      else if(hfmt.eq.'gdd') then
        call rgdard(1,hmodel,lmgeoid,geocof,36)

      else if(hfmt.eq.'cmb') then
        call rdcmb(1,hmodel,lmcmb,cmbcof)

      else if(hfmt.eq.'htm') then
        open(1,file=hmodel,status='old')
        call rdmdl(1)
        close(1)

      else if(hfmt.eq.'lmm') then
        open(1,file=hmodel,status='old')
        call rdmdll(1)
        close(1)

      else if(hfmt.eq.'pct'.or.hfmt.eq.'p58'.or.hfmt.eq.'c13'.or.hfmt.eq.'spk'.or.hfmt.eq.'sph') then
        call heread(1,hmodel,bnmdl,nstr,nlp,lmaxn,mask,lask,0)
        do i=1,nstr
          mask1(i)=1
        enddo
        do i=0,lmaxn
          lask1(i)=1
        enddo
        call mskmdl(bnmdl,nstr,lmaxn,mask,lask
     1       ,anmdl,nstr,lmaxn,mask1,lask1)

      else if(hfmt.eq.'gml') then
        open(1,file=hmodel,status='old')
        read(1,'(2i5)') kmax,lmaxn
        nstr=kmax+1
        leny=(lmaxn+1)**2
        do i=1,nstr
          do l=0,lmaxn
            read(1,'(11e12.4)') (anmdl(k+(i-1)*leny),k=l**2+1,(l+1)**2)
          enddo
        enddo
        close(1)

      else if(hfmt.eq.'mml') then
        open(1,file=hmodel,status='old')
        read(1,'(5e12.5)') cmod,cmob
        close(1)

      else if(hfmt.eq.'raw') then
        open(1,file=hmodel,status='old')
        read(1,*) lmraw
c       do i=1,(lmraw+1)**2
c         read(1,*) rawmdl(i)
c       enddo
        if(lmraw.gt.MXLRAW) stop 'Increase MXLRAW'
        read(1,*) (rawmdl(i),i=1,(lmraw+1)**2)

        read(1,*,iostat=ierrr) lmaxa
        if(ierrr.ne.0.or.lmaxa.eq.0) then
          lmaxa=0
        else
          lenva=(lmaxa-1)*(2*lmaxa+6)
          if(lenva.gt.mxyani) stop 'Increase mxyani'
          read(1,*) (yani(i),i=1,lenva)
        endif

        close(1)

      else 
        write(0,'(''spharmap: Unknown model format'')')
        call exit(2)
      endif
      

      rmoho=6346619.
      r670=5701000.
      r1500=6371000.-1500000.
      rcmb=3479958.
      rx=6371000.-depth*1000.


      if(hfmt.eq.'div') then
        lhigh=lmdiv
        do i=1,(lhigh+1)**2
          ycofs(i)=dvcof(i)
        enddo

      else if(hfmt.eq.'geo'.or.hfmt.eq.'gdd') then
        lhigh=lmgeoid
        do i=1,(lhigh+1)**2
          ycofs(i)=geocof(i)
        enddo
        if(param.eq.'grv') then
          gfac=9.81*1.e5/6371000.  ! .154: convert from metres to mgals
          k=0
          do l=0,lmgeoid
            do im=1,2*l+1
              k=k+1
              ycofs(k)=ycofs(k)*(l+1)*gfac
            enddo
          enddo
        endif

      else if(hfmt.eq.'cmb') then
        lhigh=lmcmb
        do i=1,(lhigh+1)**2
          ycofs(i)=cmbcof(i)
        enddo

      else if(hfmt.eq.'htm') then
        if(param.eq.'vs') then
          x=-1.+2.*(rx-r670)/(rmoho-r670)
          if(x.gt.1..or.x.lt.-1.) then
            write(0,'(''spharmap: htm: depth out of range'')')
            call exit(2)
          endif
          call evprem(rx,xaaa,xccc,xlll,xnnn,xfff,xrho,vn,rhobar,iq)
          vs2=2.*(vn/1000.)**2*xlll/xrho
          do icof=1,4
            vsc(icof)=pn(icof-1,x)/vs2
          enddo

          lhigh=lmaxum
          do i=1,(lhigh+1)**2
            sum=0.
            do icof=1,4
              sum=sum+vsc(icof)*pertmu((1+icof)*lenyu+i)
            enddo
            ycofs(i)=sum
          enddo
        else if(param.eq.'crust') then
          lhigh=lmaxum
          do i=1,(lhigh+1)**2
            ycofs(i)=pertmu(i)
          enddo
        else
          write(0,'(''spharmap: htm: unknown parameter'')')
          call exit(2)
        endif

      else if(hfmt.eq.'lmm') then
        if(param.eq.'vp') then
          x=-1.+2.*(rx-rcmb)/(r670-rcmb)
          if(x.gt.1..or.x.lt.-1.) then
            write(0,'(''spharmap: htm: depth out of range'')')
            call exit(2)
          endif
          call evprem(rx,xaaa,xccc,xlll,xnnn,xfff,xrho,vn,rhobar,iq)
          vs2=(vn/1000.)*sqrt(xaaa/xrho)
          do icof=1,5
            vsc(icof)=pn(icof-1,x)/vs2
          enddo

          lhigh=lmaxlm
          do i=1,(lhigh+1)**2
            sum=0.
            do icof=1,5
              sum=sum+vsc(icof)*pertml((icof-1)*lenyl+i)
            enddo
            ycofs(i)=sum
          enddo
        else
          write(0,'(''spharmap: lmm: unknown parameter'')')
          call exit(2)
        endif
      else if(hfmt.eq.'pct'.or.hfmt.eq.'p58'.or.hfmt.eq.'gml') then
        if(hfmt.eq.'pct') then
          if(param.eq.'vs'.and.rx.ge.r670.and.rx.lt.rmoho) then
            ip1=3
            ip2=6
            rtop=rmoho
            rbot=r670
          else if(param.eq.'vs'.and.rx.ge.rcmb.and.rx.lt.r670) then
            ip1=7
            ip2=ip1+nlp
            rtop=r670
            rbot=rcmb
          else if(param.eq.'vp'.and.rx.ge.r670.and.rx.lt.rmoho) then
            ip1=8+nlp
            ip2=ip1+3
            rtop=rmoho
            rbot=r670
          else if(param.eq.'vp'.and.rx.ge.rcmb.and.rx.lt.r670) then
            ip1=12+nlp
            ip2=ip1+nlp
            rtop=r670
            rbot=rcmb
          else if(param.eq.'crust') then
            ip1=1
            ip2=1
          else
            write(0,'(''spharmap: pct: unknown parameter or wrong depth'')')
            call exit(2)
          endif
        else if(hfmt.eq.'gml') then
          if(rx.ge.rcmb.and.rx.lt.r670) then
            ip1=1
            ip2=nstr
            rtop=r670
            rbot=rcmb
          else
            write(0,'(''spharmap: pct: unknown parameter or wrong depth'')')
            call exit(2)
          endif
        else if(hfmt.eq.'p58') then
          if(param.eq.'vs'.and.rx.ge.r670.and.rx.lt.rmoho) then
            ip1=3
            ip2=7
            rtop=rmoho
            rbot=r670
          else if(param.eq.'vs'.and.rx.ge.rcmb.and.rx.lt.r670) then
            ip1=8
            ip2=15
            rtop=r670
            rbot=rcmb
          else if(param.eq.'vp'.and.rx.ge.r670.and.rx.lt.rmoho) then
            ip1=16
            ip2=20
            rtop=rmoho
            rbot=r670
          else if(param.eq.'vp'.and.rx.ge.rcmb.and.rx.lt.r670) then
            ip1=21
            ip2=28
            rtop=r670
            rbot=rcmb
          else if(param.eq.'crust') then
            ip1=1
            ip2=1
          else
            write(0,'(''spharmap: pct: unknown parameter or wrong depth'')')
            call exit(2)
          endif
        endif

        lhigh=lmaxn
        if(ip1.ne.1.or.hfmt.eq.'gml') then
          x=-1.+2.*(rx-rbot)/(rtop-rbot)
          if(x.gt.1..or.x.lt.-1.) then
            write(0,'(''spharmap: pct: depth out of range'')')
            call exit(2)
          endif
          do icof=1,ip2-ip1+1
            vsc(icof)=pn(icof-1,x)
          enddo
          lenyh=(lhigh+1)**2
          do i=1,lenyh
            sum=0.
            do icof=1,ip2-ip1+1
              sum=sum+vsc(icof)*anmdl((ip1+icof-2)*lenyh+i)
            enddo
            ycofs(i)=sum
          enddo
        else
          do i=1,(lhigh+1)**2
            ycofs(i)=anmdl(i)
          enddo
        endif

      else if(hfmt.eq.'c13') then
        if(param.eq.'vs'.and.rx.ge.rcmb.and.rx.lt.rmoho) then
          ip1=3
          ip2=16
          rtop=rmoho
          rbot=rcmb
        else if(param.eq.'crust') then
          ip1=1
        else
          write(0,'(''spharmap: pct: unknown parameter or wrong depth'')')
          call exit(2)
        endif
        lhigh=lmaxn
        if(ip1.ne.1) then
          x=-1.+2.*(rx-rbot)/(rtop-rbot)
          if(x.gt.1..or.x.lt.-1.) then
            write(0,'(''spharmap: c13: depth out of range'')')
            call exit(2)
          endif
          do icof=1,ip2-ip1+1
            vsc(icof)=tn(icof-1,x)
          enddo
          lenyh=(lhigh+1)**2
          do i=1,lenyh
            sum=0.
            do icof=1,ip2-ip1+1
              sum=sum+vsc(icof)*anmdl((ip1+icof-2)*lenyh+i)
            enddo
            ycofs(i)=sum
          enddo
        else
          do i=1,(lhigh+1)**2
            ycofs(i)=anmdl(i)
          enddo
        endif

      else if(hfmt.eq.'spk') then
        if(param.eq.'vs'.and.rx.ge.rcmb.and.rx.lt.rmoho) then
          ip1=4
          ip2=24
          rtop=rmoho
          rbot=rcmb
        else if(param.eq.'crust') then
          ip1=3
        else
          write(0,'(''spharmap: pct: unknown parameter or wrong depth'')')
          call exit(2)
        endif
        lhigh=lmaxn
        lenyh=(lhigh+1)**2
        if(ip1.gt.3) then
          x=-1.+2.*(rx-rbot)/(rtop-rbot)
          if(x.gt.1..or.x.lt.-1.) then
            write(0,'(''spharmap: c13: depth out of range'')')
            call exit(2)
          endif
          do icof=1,ip2-ip1+1
            vsc(icof)=splk(icof-1,x)
          enddo
          do i=1,lenyh
            sum=0.
            do icof=1,ip2-ip1+1
              sum=sum+vsc(icof)*anmdl((ip1+icof-2)*lenyh+i)
            enddo
            ycofs(i)=sum
          enddo
        else
          do i=1,(lhigh+1)**2
            ycofs(i)=anmdl(i+(ip1-1)*lenyh)
          enddo
        endif

      else if(hfmt.eq.'sph') then
        if(param.eq.'vs'.and.rx.ge.rcmb.and.rx.lt.rmoho) then
          ip1=4
          ip2=24
          rtop=rmoho
          rbot=rcmb
        else if(param.eq.'crust') then
          ip1=3
        else
          write(0,'(''spharmap: pct: unknown parameter or wrong depth'')')
          call exit(2)
        endif
        lhigh=lmaxn
        lenyh=(lhigh+1)**2
        if(ip1.gt.3) then
          x=-1.+2.*(rx-rbot)/(rtop-rbot)
          if(x.gt.1..or.x.lt.-1.) then
            write(0,'(''spharmap: sph: depth out of range'')')
            call exit(2)
          endif
          do icof=1,ip2-ip1+1
            vsc(icof)=splh(icof-1,x)
          enddo
          do i=1,lenyh
            sum=0.
            do icof=1,ip2-ip1+1
              sum=sum+vsc(icof)*anmdl((ip1+icof-2)*lenyh+i)
            enddo
            ycofs(i)=sum
          enddo
        else
          do i=1,(lhigh+1)**2
            ycofs(i)=anmdl(i+(ip1-1)*lenyh)
          enddo
        endif



      else if(hfmt.eq.'mml') then
        lhigh=4
        ipar=0
        if(param.eq.'vp') ipar=1
        if(param.eq.'vs') ipar=2
        if(param.eq.'rho') ipar=3
        if(param.eq.'icb') ipar=4
        if(param.eq.'cmb') ipar=5
        if(param.eq.'670') ipar=6
        if(param.eq.'fsf') ipar=7
        if(ipar.eq.0) then
          write(0,'(''sparmap: format mml: unknown parameter'')')
          call exit(2)
        endif

        if(ipar.le.3) then
          call evprem(rx,xaaa,xccc,xlll,xnnn,xfff,xrho,vn,rhobar,iq)
          iyk=0
          iyke=0
          do iyl=0,4
            do iym=0,2*iyl
              iyk=1+iyk
              if(mod(iyl,2).eq.0) then
                iyke=1+iyke
                ycofs(iyk)=cmod(iyke,iq,ipar)
                if(iym.eq.0.) ycofs(iyk)=ycofs(iyk)*sqrt(.5)
              else
                ycofs(iyk)=0.
              endif
            enddo
          enddo
          if(iyk.ne.25.or.iyke.ne.15) pause 'counting error'

        else
          iyk=0
          iyke=0
          do iyl=0,4
            do iym=0,2*iyl
              iyk=1+iyk
              if(mod(iyl,2).eq.0) then
                iyke=1+iyke
                ycofs(iyk)=cmob(iyke,ipar-3)*6371.
                if(iym.eq.0.) ycofs(iyk)=ycofs(iyk)*sqrt(.5)
              else
                ycofs(iyk)=0.
              endif
            enddo
          enddo
          if(iyk.ne.25.or.iyke.ne.15) pause 'counting error'

        endif

      else if(hfmt.eq.'raw') then
        lhigh=lmraw
        do i=1,(lmraw+1)**2
          ycofs(i)=rawmdl(i)
        enddo
      else 
        write(0,'(''spharmap: Unknown model format'')')
        call exit(2)
      endif
      return
      end
c---------------------------------------------------------------------
      subroutine heread(lu,file,x,nstr,nlp,lmax,mask,lask,inorm)
      save
      character*(*) file
      dimension x(*),mask(*),lask(*)
      character*132 abuf
      character*80 form
      sqth=sqrt(.5)
      open(lu,file=file,status='old')
      read(lu,'(a132)') abuf
      llen=istlen(abuf)
      if(abuf(llen:llen).eq.'n') then
        read(abuf(llen-2:llen-1),'(i2)') nlp
        llen=istlen(abuf(1:llen-3))
      else
        nlp=4
      endif
      read(abuf,'(10x,i5)') lmax
      num=llen-15-1-(lmax+1)-1-3-1

      if(llen.gt.15) then
        idef=0
        write(form,'(''(16x,'',i3,''i1,1x,i3,1x,80i1)'')') lmax+1
        write(6,'(a30,2i8)') form(1:30),lmax,num
        read(abuf,form) (lask(i+1),i=0,lmax),nstr,(mask(i),i=1,num)
        write(*,*)'test lask', (lask(i+1),i=0,lmax)
        if(num.ne.nstr) pause 'error 1 in heread'
      else
        idef=1
        do 50 i=0,lmax
        lask(i+1)=1
   50   continue
      endif

      leny=0
      do 60 l=0,lmax
      if(lask(l+1).ne.0) leny=leny+2*l+1
   60 continue

      ip=0
  100 read(lu,'(a132)',end=99) abuf
      backspace lu
      ip=1+ip
      if(idef.ne.0) mask(ip)=1

      ind=1+(ip-1)*leny

      write(*,*)'test lmax', lmax
      do 704 l=0,lmax
      if(lask(l+1).ne.0) then
        ind1=ind+2*l
        read(lu,'(11e12.4)')(x(i),i=ind,ind1)
        write(*,*)'read lu', (x(i),i=ind,ind1)
        if(inorm.eq.1) x(ind)=x(ind)/sqth
        ind=ind1+1
      endif
  704 continue
      goto 100

   99 if(idef.ne.0) then
      nstr=ip
      nstruc=ip
      else
      nstruc=0
      do 67 i=1,nstr
      if(mask(i).ne.0) nstruc=nstruc+1
   67 continue
      write(*,*)'nstruc, ip', nstruc, ip
      if(nstruc.ne.ip) pause 'error 3 in heread'
      endif

      lenf=0
      do 22 i=1,len(file)
      ii=i
      if(file(i:i).eq.' '.or.file(i:i).eq.char(0)) goto 23
   22 continue
   23 ii=ii-1
      write(6,'(''model from'',1x,80a1)') (file(i:i),i=1,ii)
      write(6,'(i4,'' parameters: ('',i2,''n)  mask '',80i1)') nstr,nlp,(mask(i),i=1,nstr)
      write(6,'(4x,'' lmax ='',i4,'':  mask '',80i1)') lmax,(lask(i+1),i=0,lmax)
      close(lu)
      return
      end

c-----------------------------------------------------------------------
      subroutine dodec(levls,gridfile)
c
c  generates the x,y,z coordinates of the 20 vertices of a dodecohedron.
c  In the calling program x,y,z should be deimensioned 20. ipoly(i,j)
c  gives the index to the i'th vertex of the j'th pentagonal face,
c  traced in a clockwise sense, looking from the exterior. ipoly(6,j)
c  is equal to ipoly(1,j), completing the pentagon by repeating the first
c  vertex. xc(j),yc(j),zc(j) are the components of the faces' unit normals.
c
      character*(*) gridfile
      include 'dodec.h'
 
      dimension vv(3,3),indv(3)
      character*1 str1
      data levels/-1/
      if(levels.ge.levls) return
      lu=-1
c     call opnflc(lu,'/home/eeyore1/john/dta/dodec_grid',1,0,0,jsta,-1,0)
      call opnflc(lu,gridfile,1,0,0,jsta,-1,0)
      if(jsta.eq.0) then
        call bffi(lu,1,x(1,1),LENCOM*4,j,m,0)
        if(m.ne.LENCOM*4) jsta=99
        call bffi(lu,1,idum,4,j,m,0)
        if(m.ne.0.or.j.ne.3) jsta=99
        call closfl(lu,ksta)
        if(jsta.eq.0.and.levels.ge.levls) goto 999
      endif
      
      pi=4.*atan(1.)
      a72=72.*pi/180.
      a36=a72*.5
      c72=cos(a72)
      s72=sin(a72)
      c36=cos(a36)
      s36=sin(a36)
      xsid2=(1.-c72)**2+s72**2
      xsid=sqrt(xsid2)
      xv=c72*xsid2/(1.-c72)
      zv=sqrt(xsid2-xv**2)
      do i1=1,5
        a=(i1-1)*a72
        x(i1,1)=cos(a)
        y(i1,1)=sin(a)
        z(i1,1)=0.
      enddo
      do i1=1,5
        x(i1,2)=x(i1,1)*(1.+xv)
        y(i1,2)=y(i1,1)*(1.+xv)
        z(i1,2)=zv
      enddo
      do i1=1,5
        i2=1+mod(i1,5)
        i4=1+mod(i1+2,5)
        vx=.5*(x(i2,1)+x(i1,1))
        vy=.5*(y(i2,1)+y(i1,1))
        tx=.5*(x(i2,2)+x(i1,2))-vx
        ty=.5*(y(i2,2)+y(i1,2))-vy
        tz=.5*(z(i2,2)+z(i1,2))
        ta=sqrt(tx**2+ty**2+tz**2)
        tx=tx/ta
        ty=ty/ta
        tz=tz/ta
        ts=sqrt((x(i4,1)-vx)**2+(y(i4,1)-vy)**2)
        x(i1,3)=vx+ts*tx
        y(i1,3)=vy+ts*ty
        z(i1,3)=ts*tz
      enddo


      do i1=1,5
        i2=1+mod(i1,5)
        i3=1+mod(i1+1,5)
        i4=1+mod(i1+2,5)
        alph=c72*xsid2
     1     /((x(i1,1)+x(i2,1))*(x(i4,1)-x(i3,1))+(y(i1,1)+y(i2,1))*(y(i4,1)-y(i3,1)))
        tx=alph*(x(i1,1)+x(i2,1))
        ty=alph*(y(i1,1)+y(i2,1))
        tz=sqrt(xsid2-tx**2-ty**2)
        x(i1,4)=x(i1,3)+tx
        y(i1,4)=y(i1,3)+ty
        z(i1,4)=z(i1,3)+tz
      enddo
      tx=0.
      ty=0.
      tz=0.
      do i=1,5
      do j=1,4
        tx=tx+x(i,j)
        ty=ty+y(i,j)
        tz=tz+z(i,j)
      enddo
      enddo
      tx=tx/20.
      ty=ty/20.
      tz=tz/20.
      do i=1,5
      do j=1,4
        x(i,j)=x(i,j)-tx
        y(i,j)=y(i,j)-ty
        z(i,j)=z(i,j)-tz
        s=sqrt(x(i,j)**2+y(i,j)**2+z(i,j)**2)
        x(i,j)=x(i,j)/s
        y(i,j)=y(i,j)/s
        z(i,j)=z(i,j)/s
      enddo
      enddo
      ipoly(1,1)=1
      ipoly(2,1)=2
      ipoly(3,1)=3
      ipoly(4,1)=4
      ipoly(5,1)=5
      ipoly(6,1)=1

      ipoly(1,2)=11
      ipoly(2,2)=7
      ipoly(3,2)=2
      ipoly(4,2)=1
      ipoly(5,2)=6
      ipoly(6,2)=11

      ipoly(1,3)=12
      ipoly(2,3)=8
      ipoly(3,3)=3
      ipoly(4,3)=2
      ipoly(5,3)=7
      ipoly(6,3)=12

      ipoly(1,4)=13
      ipoly(2,4)=9
      ipoly(3,4)=4
      ipoly(4,4)=3
      ipoly(5,4)=8
      ipoly(6,4)=13

      ipoly(1,5)=14
      ipoly(2,5)=10
      ipoly(3,5)=5
      ipoly(4,5)=4
      ipoly(5,5)=9
      ipoly(6,5)=14

      ipoly(1,6)=15
      ipoly(2,6)=6
      ipoly(3,6)=1
      ipoly(4,6)=5
      ipoly(5,6)=10
      ipoly(6,6)=15

      ipoly(1,7)=6
      ipoly(2,7)=15
      ipoly(3,7)=20
      ipoly(4,7)=16
      ipoly(5,7)=11
      ipoly(6,7)=6

      ipoly(1,8)=7
      ipoly(2,8)=11
      ipoly(3,8)=16
      ipoly(4,8)=17
      ipoly(5,8)=12
      ipoly(6,8)=7

      ipoly(1,9)=8
      ipoly(2,9)=12
      ipoly(3,9)=17
      ipoly(4,9)=18
      ipoly(5,9)=13
      ipoly(6,9)=8

      ipoly(1,10)=9
      ipoly(2,10)=13
      ipoly(3,10)=18
      ipoly(4,10)=19
      ipoly(5,10)=14
      ipoly(6,10)=9

      ipoly(1,11)=10
      ipoly(2,11)=14
      ipoly(3,11)=19
      ipoly(4,11)=20
      ipoly(5,11)=15
      ipoly(6,11)=10

      ipoly(1,12)=18
      ipoly(2,12)=17
      ipoly(3,12)=16
      ipoly(4,12)=20
      ipoly(5,12)=19
      ipoly(6,12)=18

      do ip=1,12
        xc(ip)=0.
        yc(ip)=0.
        zc(ip)=0.
        do iv=1,5
          xc(ip)=xc(ip)+x1(ipoly(iv,ip))
          yc(ip)=yc(ip)+y1(ipoly(iv,ip))
          zc(ip)=zc(ip)+z1(ipoly(iv,ip))
        enddo
        ss=sqrt(xc(ip)**2+yc(ip)**2+zc(ip)**2)
        xc(ip)=xc(ip)/ss
        yc(ip)=yc(ip)/ss
        zc(ip)=zc(ip)/ss
        if(ip.eq.1) then
          vec1(1,ip)=1.
          vec1(2,ip)=0.
          vec1(3,ip)=0.
          vec2(1,ip)=0.
          vec2(2,ip)=1.
        else if(ip.ge.2.and.ip.le.6) then
          ph=atan2(yc(ip),xc(ip))
          th=acos(zc(ip))
          vec1(1,ip)=-cos(th)*cos(ph)
          vec1(2,ip)=-cos(th)*sin(ph)
          vec1(3,ip)=sin(th)
          vec2(1,ip)=-sin(ph)
          vec2(2,ip)=cos(ph)
        else if(ip.ge.7.and.ip.le.11) then
          ph=atan2(yc(ip),xc(ip))
          th=acos(zc(ip))
          vec1(1,ip)=cos(th)*cos(ph)
          vec1(2,ip)=cos(th)*sin(ph)
          vec1(3,ip)=-sin(th)
          vec2(1,ip)=sin(ph)
          vec2(2,ip)=-cos(ph)
        else if(ip.eq.12) then
          vec1(1,ip)=-1.
          vec1(2,ip)=0.
          vec1(3,ip)=0.
          vec2(1,ip)=0.
          vec2(2,ip)=1.
        else 
          pause 'dodec: poly error'
        endif
      enddo

      nv=0
      do i=1,20
        nv=nv+1
        vertcs(1,nv)=x1(i)
        vertcs(2,nv)=y1(i)
        vertcs(3,nv)=z1(i)
      enddo
      do i=1,12
        nv=nv+1
        vertcs(1,nv)=xc(i)
        vertcs(2,nv)=yc(i)
        vertcs(3,nv)=zc(i)
      enddo
      nt=0
      do i=1,12
        do j=1,5
          nt=nt+1
          ivert(1,nt)=20+i
          ivert(2,nt)=ipoly(j,i)
          ivert(3,nt)=ipoly(j+1,i)
        enddo
      enddo
      
      nlev=0
      nt0(nlev)=0
  100 continue
      nv0(nlev)=nv
      nt0(nlev+1)=nt
      if(nlev.ge.levls) goto 99
      do it=nt0(nlev)+1,nt0(nlev+1)
        do is=1,3
          is1=1+mod(is,3)
          do k=1,3
            vv(k,is)=.5*(vertcs(k,ivert(is,it))+vertcs(k,ivert(is1,it)))
          enddo
          ss=0
          do k=1,3
            ss=ss+vv(k,is)**2
          enddo
          ss=1./sqrt(ss)
          do k=1,3
            vv(k,is)=vv(k,is)*ss
          enddo
        enddo
        do is=1,3
          is1=is-1
          if(is1.eq.0) is1=3
          poles(1,is,it)=vv(2,is)*vv(3,is1)-vv(3,is)*vv(2,is1)
          poles(2,is,it)=vv(3,is)*vv(1,is1)-vv(1,is)*vv(3,is1)
          poles(3,is,it)=vv(1,is)*vv(2,is1)-vv(2,is)*vv(1,is1)
          ss=0
          do k=1,3
            ss=ss+poles(k,is,it)**2
          enddo
          ss=1./sqrt(ss)
          do k=1,3
            poles(k,is,it)=poles(k,is,it)*ss
          enddo
        enddo

        do is=1,3
     
          do iv=1,nv
            if(vv(1,is).eq.vertcs(1,iv)
     1        .and.vv(2,is).eq.vertcs(2,iv)
     1        .and.vv(3,is).eq.vertcs(3,iv)) then
              indv(is)=iv
              goto 33
            endif
          enddo
          nv=nv+1
          do k=1,3
            vertcs(k,nv)=vv(k,is)
          enddo
          indv(is)=nv
   33     continue
        enddo
        isons(it)=nt
        nt=nt+1
        ivert(1,nt)=ivert(1,it)
        ivert(2,nt)=indv(1)
        ivert(3,nt)=indv(3)
        nt=nt+1
        ivert(1,nt)=ivert(2,it)
        ivert(2,nt)=indv(2)
        ivert(3,nt)=indv(1)
        nt=nt+1
        ivert(1,nt)=ivert(3,it)
        ivert(2,nt)=indv(3)
        ivert(3,nt)=indv(2)
        nt=nt+1
        ivert(1,nt)=indv(2)
        ivert(2,nt)=indv(3)
        ivert(3,nt)=indv(1)
      enddo
      nlev=1+nlev
      goto 100
   99 continue
      levels=levls
      lu=-1
      call opnflc(lu,gridfile,4,0,0,jsta,-1,2)
      call bffo(lu,1,x(1,1),LENCOM*4,j,0)
      call closfl(lu,ksta)

  999 continue

cc this code writes out files for mathematica
cc      do il=0,levls
cc        write(str1,'(i1)') il
cc        open(93,file='dodec.'//str1//'.ma')
cc        write(93,'(a)') '{'
cc        do it=nt0(il),nt0(il+1)-1
cc          if(it.ne.nt0(il+1)-1)
cc     1    write(93,'(''Polygon[{'',2( ''{'',2(f7.4,'',''),f7.4,''}, '')
cc     1             ,''{'',2(f7.4,'',''),f7.4,''} }],''
cc     1      )')
cc     1      ((vertcs(j,ivert(i,it+1)),j=1,3),i=1,3)
cc          if(it.eq.nt0(il+1)-1)
cc     1    write(93,'(''Polygon[{'',2( ''{'',2(f7.4,'',''),f7.4,''}, '')
cc     1             ,''{'',2(f7.4,'',''),f7.4,''} }] }''
cc     1      )')
cc     1      ((vertcs(j,ivert(i,it+1)),j=1,3),i=1,3)
cc        enddo
cc        close(93)
cc      enddo

      return
      end

      subroutine getdodp(lev,nvert,ntri)
      include 'dodec.h'
      ntri=nt0(lev+1)-nt0(lev)
      nvert=nv0(lev)
      return
      end

c---------------------------------------------------
      subroutine plotani(yani,lmaxa,scla,lvdodec,alphd,betad,gamad
     1 ,it1,it2,jt1,jt2,x1i,x2i,y1i,y2i,iproj,mapprj)
      dimension yani(*)
      include 'dodec.h'
      dimension urot(3,3)
      parameter (MXLA=20)
      double precision d((2*MXLA+1)**2)
      dimension ypp((MXLA-1)*(2*MXLA+6)),ytp((MXLA-1)*(2*MXLA+6))
     1    ,ylms((MXLA+1)**2)

      lenva=(lmaxa-1)*(2*lmaxa+6)

      rad=45./atan(1.)
      call setrot(alphd/rad,betad/rad,gamad/rad,urot)
      call twindo(it1,it2,jt1,jt2)
      call dwindo(x1i,x2i,y1i,y2i)
      call linwdt(5)
      call lincol(62)
      do iv=1,nv0(lvdodec)
         xx0=rad*atan2(vertcs(2,iv),vertcs(1,iv))
         yy0=90.-rad*acos(vertcs(3,iv))
         xx=xx0
         yy=yy0
         call rotll(yy,xx,yy,xx,urot)
         if(xx.lt.0.) xx=xx+360.
         call mapprj(xx,yy,iproj,xp,yp)

         call ylmv(yy0,xx0,lmaxa,ylms,ypp,ytp,d)

         cpp=0.
         ctp=0.
         do i=1,lenva
           cpp=cpp+ypp(i)*yani(i)
           ctp=ctp+ytp(i)*yani(i)
         enddo
         xmag=sqrt(cpp**2+ctp**2)
cwrong        xazm=rad*atan2(ctp,-xmag-cpp)
         xazm=rad*atan2(xmag+cpp,-ctp)

         call pdaz(yy0,xx0,xazm,2.,yy1,xx1)
         xx=xx1
         yy=yy1
         call rotll(yy,xx,yy,xx,urot)
         if(xx.lt.0.) xx=xx+360.
         call mapprj(xx,yy,iproj,xp1,yp1)

         call c_dviusr(xp,yp,xs,ys)
         call c_dviusr(xp1,yp1,xs1,ys1)
         scrl=xmag*100.*scla

         fac=scrl/sqrt( (xs1-xs)**2+(ys1-ys)**2 )
         ixa=xs+(xs1-xs)*fac+.5
         iya=ys+(ys1-ys)*fac+.5
         ixb=xs-(xs1-xs)*fac+.5
         iyb=ys-(ys1-ys)*fac+.5
         call movabs(ixa,iya)
         call drwabs(ixb,iyb)
         call tsend()
c        write(6,*) xmag,xazm,yy0,xx0,yy1,xx1,xs,ys,xs1,ys1,ixa,iya,ixb,iyb

      enddo
      call tsend()

      return
      end
c--------------------------------------------------
      subroutine grdscl(grid,igrid,ikla,iklo,zmin,zmax)
      dimension grid(igrid,1)
      zmin1=zmin+.001*(zmax-zmin)
      zmax1=zmax-.001*(zmax-zmin)
      do 100 ilon=1,iklo
      z=zmin1+(float(ilon-1))*(zmax1-zmin1)/float(iklo-1)
      do 100 ilat=1,ikla
      grid(ilat,ilon)=z
  100 continue
      return
      end
c---------------------------------------------------------------------
      subroutine rgdard(lu,file,lmgeoid,geocof,lcut)
      character*(*) file
      dimension geocof(*)
c     fac1=6371000.*1.e-6
      fac1=6371000.
      fac2=fac1*sqrt(4.*3.1415926)
      fac3=fac2*sqrt(2.)
      lmgeoid=-1
      open(lu,file=file,status='old')
      k=0
   10 read(lu,'(6x,2i2,2e15.8)',end=20) l,m,val1,val2
      if(l.gt.lcut) goto 10
      if(l.eq.2.and.m.eq.0) val1=val1+0.000479865
      lmgeoid=max0(lmgeoid,l)
      k=l**2+2*m
      if(m.eq.0) then
        geocof(k+1)=val1*fac2
      else
        if(mod(m,2).eq.0) then
          fac=fac3
        else
          fac=-fac3
        endif
        geocof(k)=val1*fac
        geocof(k+1)=val2*fac
      endif
      goto 10
   20 continue
      close(lu)
      return
      end

pro blobzoo3

restore,'blobzoo.sav'
firstcloud=0
working=1
mouse=1
keyboard=0
smoothysize=4


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build Template Structure ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

template=[{cube:[0,0],$
	kern_num:0,$
        rm_num:0,$
        inspected:0,$
        glitch:0,$
        connected:0,$
        complex:0,$
        good:0,$
        dwarf:0,$
        ra:0.,$
        dec:0.,$
        vel:0.}]

;;;;;;;;;;;;;;;;;;;;;;
;;; Identify User ;;;;
;;;;;;;;;;;;;;;;;;;;;;

; Find names of previous viewers ;

viewers=blobzoo.viewers[0:blobzoo.n_viewers-1]
n_viewers=n_elements(viewers)

; Check to see that the structures are there for each viewer.
; And build fullstatus array

fullstatus=fltarr(45,5)
for i=0,n_viewers-1 do begin

check=file_exists(viewers[i]+'_zoo1.sav')
if check eq 0 then print,'Caution: Missing '+viewers[i]+'_z001.sav, will create new file if '+viewers[i]+' is selected.' else begin
restore,viewers[i]+'_zoo1.sav'
fullstatus-=(zoo1.status < 0)
endelse

check=file_exists(viewers[i]+'_zoo2.sav')
if check eq 0 then print,'Caution: Missing '+viewers[i]+'_zoo2.sav, will create new file if '+viewers[i]+' is selected.'
endfor

viewers=[viewers,'New Viewer']

; Use Buttons for Viewer Selection ;
window,xsize=1000,ysize=500
userwin=!d.window
v_indx=b_buttons(viewers,asp=0.5,win=userwin)
name=viewers[v_indx]

; Allow for Creation of New Viewer ;

if v_indx eq n_viewers then begin
read,'Type the Name of the New Viewer: ',name
name=strlowcase(name)
blobzoo.viewers[n_viewers]=name
blobzoo.n_viewers+=1
save,blobzoo,f='blobzoo.sav'
endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Build Viewer Structures ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

check=file_exists(name+'_zoo1.sav')
if check eq 0 then begin
zoo1={name:name,$
	status:intarr(45,5)}	
save,zoo1,f=name+'_zoo1.sav'
endif else begin
restore,name+'_zoo1.sav',/verb
endelse

check=file_exists(name+'_zoo2.sav')
if check eq 0 then begin
zoo2=template
firstcloud=1
;save,zoo2,f=name+'_zoo2.sav'
endif else begin
restore,name+'_zoo2.sav'
endelse



while working eq 1 do begin

workingoncube=1
direction=1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Select Cube ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cubeselect=0
while cubeselect eq 0 do begin

print,'Pick a Cube to Work on. Black:Unseen, Blue:Finished, Red:In Progress.'
wait,0.5
cube=blobskystatus(zoo1.status,fullstatus)
cube=fix(cube)

suffix=b_cname(cube[0],cube[1])

check=file_exists('pig_fit'+suffix+'.sav')
if check eq 0 then print,'No Data for that Cube, Please Pick Again'

cubeselect=check
endwhile

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load Current Cube ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
restore,'spores'+suffix+'.sav'
restore,'datacoords'+suffix+'.sav'
spore=fits

fits=0b
nx=48
sz=size(spore)
;spore=spore[nx/2:sz[1]-nx/2-1,nx/2:sz[2]-nx/2-1,*]
nanns=where(finite(spore) eq 0,ct)
if ct gt 0 then spore[nanns]=0
nanns=where(spore lt -30,ct)
if ct gt 0 then spore[nanns]=0
szrr=size(ra)
ra=ra[30:szrr[1]-31,30:szrr[2]-31]
dec=dec[30:szrr[1]-31,30:szrr[2]-31]
sporesz=size(spore)

springcheck=file_exists('/share/galfa/html/DR1-S/Wide/GALFA_HI_RA+DEC'+suffix+'_W.fits.gz')
;fallcheck=file_exists('/share/galfa/fallmrg/merged/sct/DR1-F/GALFA_HI_RA+DEC'+suffix+'_W.fits.gz')
if springcheck then begin
;extract_coords,'/share/galfa/fallmrg/merged/sct/DR1-F/GALFA_HI_RA+DEC'+suffix+'_W.fits.gz',ra,dec,vels,fits
extract_coords,'/share/galfa/html/DR1-S/Wide/GALFA_HI_RA+DEC'+suffix+'_W.fits.gz',ora,odec,vels,fits
endif else begin
extract_coords,'/share/galfa/fallmrg/merged/sct/DR1-F/GALFA_HI_RA+DEC'+suffix+'_W.fits.gz',ora,odec,vels,fits
endelse
onanns=where(finite(fits) eq 0 or fits lt -30,ct)
nanns=where(finite(fits[*,*,1000]) eq 0 or fits[*,*,1000] le -30,ctnan)
if ct gt 0 then fits[onanns]=0
ora=float(ora)
odec=float(odec)
vels=float(vels)
fitssz=size(fits)
naninfo=fits[*,*,1000]*0
if ctnan gt 0 then begin
naninfo[nanns]=1
ctnanx=nanns mod fitssz[1]
ctnany=nanns/fitssz[2]
naninfo[(ctnanx+1)<(fitssz[1]-1),ctnany]=1
naninfo[(ctnanx-1)>0,ctnany]=1
naninfo[ctnanx,(ctnany+1)<(fitssz[2]-1)]=1
naninfo[ctnanx,(ctnany-1)>0]=1
;naninfo[(ctnanx+2)<(fitssz[1]-1),ctnany]=1
;naninfo[(ctnanx-2)>0,ctnany]=1
;naninfo[ctnanx,(ctnany+2)<(fitssz[2]-1)]=1
;naninfo[ctnanx,(ctnany-2)>0]=1
endif


restore,'pig_fit'+suffix+'.sav.new'
primaryinx=where((pig_fit.primary eq 1) and (pig_fit.embed eq 0),ct)
pig_fit=pig_fit[primaryinx]


chin=[1250,1800]
noisefind,spore,noise2d_pos,chin=chin
noisefind,spore,noise2d_neg,chin=chin-1024
noise2d=noise2d_pos<noise2d_neg



n_blobs=n_elements(pig_fit)
print,'There are '+strtrim(string(n_blobs),1)+' blobs in this cube.'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Decide Which Blob To Start On ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if zoo1.status[cube[0],cube[1]] eq 0 then begin
print, "You Haven't Examined This Cube"
print, 'Starting on the first blob.'
currentblob=0
endif

if zoo1.status[cube[0],cube[1]] eq -1 then begin
pickblob=0
print,'You Have Already Seen All Blobs in this Cube'
while pickblob eq 0 do begin
currentblob=''
read,'Which Blob Do you want to Start On? (Default=0):',currentblob
if currentblob eq '' then currentblob=0
currentblob=fix(currentblob>0)
if currentblob lt n_blobs-1 then pickblob=1
if currentblob ge n_blobs then print,'There are not that many blobs in this cube, pick again.'
endwhile
endif

if zoo1.status[cube[0],cube[1]] gt 0 then begin
pickblob=0
print,'You Have Examined '+strtrim(string(fix(zoo1.status[cube[0],cube[1]])),1)+' of '+strtrim(string(n_blobs),1)+' blobs in this Cube.'
while pickblob eq 0 do begin
currentblob=''
read,'Which Blob Do you want to Start On? (Default='+strtrim(string(fix(zoo1.status[cube[0],cube[1]])),1)+'):',currentblob
if currentblob eq '' then currentblob=zoo1.status[cube[0],cube[1]]
currentblob=fix(currentblob>0)
if currentblob lt n_blobs-1 then pickblob=1
if currentblob gt zoo1.status[cube[0],cube[1]] then begin
print,"I can't allow you to skip blobs, starting on blob "+strtrim(string(zoo1.status[cube[0],cube[1]]),1)
currentblob=zoo1.status[cube[0],cube[1]]
endif
endwhile
endif



; Find Blob in zoo2, or add new structure to zoo2, careful about first clouds ;


while workingoncube eq 1 do begin

j=where((zoo2.kern_num eq pig_fit[currentblob].kern_num) and (zoo2.rm_num eq pig_fit[currentblob].rm_num) and (zoo2.cube[0] eq cube[0]) and (zoo2.cube[1] eq cube[1]))



if j eq -1 then j=n_elements(zoo2)
if firstcloud eq 1 then begin
j=0
firstcloud=0
endif
if j eq n_elements(zoo2) then zoo2=[zoo2,template]

; Fill in easy stuff
zoo2[j].ra=pig_fit[currentblob].ra
zoo2[j].dec=pig_fit[currentblob].dec
zoo2[j].vel=pig_fit[currentblob].v
zoo2[j].cube=cube
zoo2[j].rm_num=pig_fit[currentblob].rm_num
zoo2[j].kern_num=pig_fit[currentblob].kern_num

stmax=480
vels=findgen(2048)-2048./2.
vels*=0.736122839
scrap=min(abs(ra[*,0]-pig_fit[currentblob].ra),xmax)
scrap=min(abs(dec[0,*]-pig_fit[currentblob].dec),ymax)
scrap=min(abs(vels-pig_fit[currentblob].v),vmax)
x1=((xmax)-(stmax mod 31))>0
x2=((xmax)+30-(stmax mod 31))<(sporesz[1]-1)
y1=((ymax)-(stmax/31))>0
y2=((ymax)+30-(stmax/31))<(sporesz[2]-1)
scrap=min(abs(ora[*,0]-pig_fit[currentblob].ra),oxmax)
scrap=min(abs(odec[0,*]-pig_fit[currentblob].dec),oymax)
ox1=((oxmax)-(stmax mod 31))>0
ox2=((oxmax)+30-(stmax mod 31))<(fitssz[1]-1)
oy1=((oymax)-(stmax/31))>0
oy2=((oymax)+30-(stmax/31))<(fitssz[2]-1)

presub=fits[oxmax,oymax,vmax]
postsub=spore[xmax,ymax,vmax]
galsub=presub-postsub





;;;;;;;;;;;;;;;;;;;;
; Build the Window ;
;;;;;;;;;;;;;;;;;;;;
workingonblob=1

;;;;;;; GLitch +-700 ;;;;;;;;
if abs(zoo2[j].vel) ge 650 then begin
zoo2[j].glitch=1
endif 

if (abs(zoo2[j].vel) ge 675) then begin
workingonblob=0
currentblob+=direction
currentblob=currentblob>0
j+=1
endif else begin
if galsub gt 1.0 then begin
zoo2[j].connected=1
workingonblob=0
currentblob+=direction
currentblob=currentblob>0
j+=1
endif else begin
if (total(pig_fit[currentblob].psrmask*naninfo[ox1:ox2,oy1:oy2]) gt 0) then begin
zoo2[j].glitch=1
;currentblob+=direction
;currentblob=currentblob>0
;workingonblob=0
endif
endelse
endelse




while workingonblob eq 1 do begin

window,2,xs=400,ys=400,retain=2
xyouts,0.1,0.85,'Ra = '+string(zoo2[j].ra),charsize=3,/norm
xyouts,0.1,0.75,'Dec = '+string(zoo2[j].dec),charsize=3,/norm
xyouts,0.1,0.65,'V = '+string(zoo2[j].vel),charsize=3,/norm
xyouts,0.1,0.55,'Rmask Num = '+string(zoo2[j].rm_num),charsize=3,/norm
xyouts,0.1,0.45,'T_bmax = '+string(pig_fit[currentblob].t_bmax),charsize=3,/norm
xyouts,0.1,0.35,'Blob '+string(currentblob)+' of '+string(n_blobs),charsize=3,/norm

window,xs=1050,ys=1400,retain=2
mainwin=!d.window
xs=!d.x_size
ys=!d.y_size
;loadct,0,/silent


finitestamp=pig_fit[currentblob].postagestamp
nanns=where(finite(finitestamp) eq 0,ct)
if ct gt 0 then finitestamp[nanns]=0

pstamp=congrid(finitestamp,300,300)
pmask=congrid(pig_fit[currentblob].psrmask,300,300)
pstamp=bytscl(pstamp,min=min(pstamp*pmask),max=max(pstamp*pmask))
p_vels=vels[pig_fit[currentblob].pv[0]:pig_fit[currentblob].pv[1]]


p_spec=fltarr(31,31,2048)
p_spec[(30-(ox2-ox1))*(ox2 ne fitssz[1]-1):(ox2-ox1)+(30-(ox2-ox1))*(ox2 ne fitssz[1]-1),(30-(oy2-oy1))*(oy2 ne fitssz[2]-1):(oy2-oy1)+(30-(oy2-oy1))*(oy2 ne fitssz[2]-1),*]=fits[ox1:ox2,oy1:oy2,*]
p_spec2=p_spec
p_spec=p_spec*rebin(pig_fit[currentblob].psrmask,31,31,2048)
p_spec=total(total(p_spec,1),1)

p_spec3=p_spec[pig_fit[currentblob].pv[0]:pig_fit[currentblob].pv[1]]
pspecmin=min(p_spec[pig_fit[currentblob].pv[0]-5:pig_fit[currentblob].pv[1]+5])
pspecmax=max(p_spec[pig_fit[currentblob].pv[0]-5:pig_fit[currentblob].pv[1]+5])

loadct,2,/silent
plot,vels,p_spec,pos=[0.1,0.45,0.5,0.6],/ysty,/xsty,$
	xran=[vels[(pig_fit[currentblob].pv[0]-30)>0],vels[(pig_fit[currentblob].pv[1]+30)<2047]],yran=[pspecmin,pspecmax],/noerase,charsize=2,font=-1
oplot,p_vels,p_spec3,color=100
loadct,0,/silent


;;; Pull cube containing cloud
blobcube=fltarr(31,31,pig_fit[currentblob].pv[1]-pig_fit[currentblob].pv[0]+1)
blobcube[(30-(ox2-ox1))*(ox2 ne fitssz[1]-1):(ox2-ox1)+(30-(ox2-ox1))*(ox2 ne fitssz[1]-1),(30-(oy2-oy1))*(oy2 ne fitssz[2]-1):(oy2-oy1)+(30-(oy2-oy1))*(oy2 ne fitssz[2]-1),*]=fits[ox1:ox2,oy1:oy2,pig_fit[currentblob].pv[0]:pig_fit[currentblob].pv[1]]
bcsz=size(blobcube)
nanns=where(blobcube lt -30,ct)
if ct gt 0 then blobcube[nanns]=0

loadct,0,/silent
finitestamp=total(blobcube,3)
pstamp=congrid(finitestamp,300,300)
pmask=congrid(pig_fit[currentblob].psrmask,300,300)
pstampmin=pstamp*pmask
stampminind=where(pstampmin ne 0,ctt)
if ctt eq 0 then stampmin=0 else begin
pstampmin=pstampmin[where(pstampmin ne 0)]
endelse
pstamp=bytscl(pstamp,min=min(pstampmin),max=max(pstampmin))
tv,pstamp,.367,.65,/norm
contour,pmask,pos=[0.367,0.65,0.367+300./xs,0.65+300./ys],xmarg=[0,0],ymarg=[0,0],/noerase,/ysty,/xsty




blobcube=smooth(blobcube,[1,1,smoothysize])
cruisemin=min(blobcube*rebin(pig_fit[currentblob].psrmask,31,31,bcsz(3)))
cruisemax=max(blobcube*rebin(pig_fit[currentblob].psrmask,31,31,bcsz(3)))
x11=(x1-30)>0
x22=(x2+30)<(sporesz[1]-1)
y11=(y1-30)>0
y22=(y2+30)<(sporesz[2]-1)
ox11=(ox1-30)>0
ox22=(ox2+30)<(fitssz[1]-1)
oy11=(oy1-30)>0
oy22=(oy2+30)<(fitssz[2]-1)

blobcube=fits[ox11:ox22,oy11:oy22,pig_fit[currentblob].pv[0]-smoothysize:pig_fit[currentblob].pv[1]+smoothysize]
bcsz=size(blobcube)
nanns=where(blobcube lt -30,ct)
if ct gt 0 then blobcube[nanns]=0
blobcube=smooth(blobcube,[1,1,smoothysize])

wholestamp=fits[*,*,vmax]
nanns=where(fits[*,*,vmax] lt -30,ct)
if ct gt 0 then wholestamp[nanns]=0
wholestamp=congrid(wholestamp,300,300)
wholestamp=bytscl(wholestamp)
tv,wholestamp,.05,0.65,/norm
plot,[0,fitssz[1]],[0,fitssz[2]],/nodata,/xsty,/ysty,pos=[0.05,0.65,0.05+300./xs,0.65+300./ys],/noerase
plots,[ox1,ox1,ox2,ox2,ox1],[oy1,oy2,oy2,oy1,oy1]


;; find galsub total spectrum
gsblobcube=fltarr(31,31,pig_fit[currentblob].pv[1]-pig_fit[currentblob].pv[0]+1)
gsblobcube[(30-(x2-x1))*(x2 ne sporesz[1]-1):(x2-x1)+(30-(x2-x1))*(x2 ne sporesz[1]-1),(30-(y2-y1))*(y2 ne sporesz[2]-1):(y2-y1)+(30-(y2-y1))*(y2 ne sporesz[2]-1),*]=spore[x1:x2,y1:y2,pig_fit[currentblob].pv[0]:pig_fit[currentblob].pv[1]]
gsstamp=total(gsblobcube,3)
bcsz=size(gsblobcube)

gsspecint=fltarr(31,31,2048)
gsspecint[(30-(x2-x1))*(x2 ne sporesz[1]-1):(x2-x1)+(30-(x2-x1))*(x2 ne sporesz[1]-1),(30-(y2-y1))*(y2 ne sporesz[2]-1):(y2-y1)+(30-(y2-y1))*(y2 ne sporesz[2]-1),*]=spore[x1:x2,y1:y2,*]
gsspeccube=gsspecint
gsspecint=gsspecint*rebin(pig_fit[currentblob].psrmask,31,31,2048)
gsspecint=total(total(gsspecint,1),1)


gsblobcube=smooth(gsblobcube,[1,1,smoothysize])
gscruisemin=min(gsblobcube*rebin(pig_fit[currentblob].psrmask,31,31,bcsz(3)))
gscruisemax=max(gsblobcube*rebin(pig_fit[currentblob].psrmask,31,31,bcsz(3)))

gsblobcube=spore[x11:x22,y11:y22,pig_fit[currentblob].pv[0]-smoothysize:pig_fit[currentblob].pv[1]+smoothysize]
bcsz=size(gsblobcube)

gsblobcube=smooth(gsblobcube,[1,1,smoothysize])

gsstamp=congrid(gsstamp,300,300)
gstampnoise=congrid(noise2d[x11:x22,y11:y22],300,300)

gsstampmin=gsstamp*pmask
gstampminind=where(gsstampmin ne 0,ctt)
if ctt eq 0 then gsstampmin=0 else begin
gsstampmin=gsstampmin[where(gsstampmin ne 0)]
endelse
gsstampol=gsstamp
gstampnoise=sqrt(gstampnoise^2*(pig_fit[currentblob].pv[1]-pig_fit[currentblob].pv[0]))
gsstamp=bytscl(gsstamp,min=min(gsstampmin),max=max(gsstampmin))
loadct,0,/silent
tv,gsstamp,.7,.65,/norm
contour,pmask,pos=[0.7,0.65,0.7+300./xs,0.65+300./ys],xmarg=[0,0],ymarg=[0,0],/noerase,/ysty,/xsty
loadct,2,/silent
;contour,gsstampol,pos=[0.7,0.65,0.7+300./xs,0.65+300./ys],xmarg=[0,0],ymarg=[0,0],/noerase,/ysty,/xsty,levels=[3*sqrt(0.1^2*(pig_fit[currentblob].pv[1]-pig_fit[currentblob].pv[0]))],color=100,thick=3
contour,gsstampol/gstampnoise,pos=[0.7,0.65,0.7+300./xs,0.65+300./ys],xmarg=[0,0],ymarg=[0,0],/noerase,/ysty,/xsty,levels=[5],color=100,thick=3

print,3*sqrt((0.1^2)*(pig_fit[currentblob].pv[1]-pig_fit[currentblob].pv[0]))

p_vels=vels[pig_fit[currentblob].pv[0]:pig_fit[currentblob].pv[1]]
gsspecint2=gsspecint[pig_fit[currentblob].pv[0]:pig_fit[currentblob].pv[1]]
gsspecmin=min(gsspecint[pig_fit[currentblob].pv[0]-5:pig_fit[currentblob].pv[1]+5])
gsspecmax=max(gsspecint[pig_fit[currentblob].pv[0]-5:pig_fit[currentblob].pv[1]+5])
plot,vels,gsspecint,pos=[0.55,0.45,0.95,0.6],/ysty,/xsty,$
        xran=[vels[(pig_fit[currentblob].pv[0]-30)>0],vels[(pig_fit[currentblob].pv[1]+30)<2047]],yran=[gsspecmin,gsspecmax],/noerase,charsize=2,font=-1
oplot,p_vels,gsspecint2,color=100

loadct,0,/silent




window,/free,/pixmap,xsize=xs,ysize=350
buttonwin=!d.window
buttons=1
while buttons eq 1 do begin

if mouse eq 1 then begin
wset,buttonwin
plot,[0,14],[0,9],/xsty,/ysty,/nodata,ticklen=0,xmarg=[0,0],ymarg=[0,0.1],thick=3

xyouts,10,7,'Gal Sub='+string(galsub),charsize=2.5

if zoo2[j].good eq 1 then polyfill,[1,1,4,4,1],[7,8,8,7,7],color=100,thick=2
plots,[1,1,4,4,1],[7,8,8,7,7],thick=2
xyouts,2.5,7.25,'Good',align=0.5,charsize=3.5

if zoo2[j].complex eq 1 then polyfill,[5,5,8,8,5],[7,8,8,7,7],color=100,thick=2
plots,[5,5,8,8,5],[7,8,8,7,7],thick=2
xyouts,6.5,7.25,'Complex',align=0.5,charsize=3.5

if zoo2[j].connected eq 1 then polyfill,[1,1,4,4,1],[5,6,6,5,5],color=100,thick=2
plots,[1,1,4,4,1],[5,6,6,5,5],thick=2
xyouts,2.5,5.25,'Connected',align=0.5,charsize=3.5

if zoo2[j].glitch eq 1 then polyfill,[5,5,8,8,5],[5,6,6,5,5],color=100,thick=2
plots,[5,5,8,8,5],[5,6,6,5,5],thick=2
xyouts,6.5,5.25,'Glitch',align=0.5,charsize=3.5

plots,[1,1,4,4,1],[3,4,4,3,3],thick=2
xyouts,2.5,3.25,'Previous Blob',align=0.5,charsize=3.5

plots,[5,5,8,8,5],[3,4,4,3,3],thick=2
xyouts,6.5,3.25,'Next Blob',align=0.5,charsize=3.5

plots,[1,1,4,4,1],[1,2,2,1,1],thick=2
xyouts,2.5,1.25,'Exit Cube',align=0.5,charsize=3.5

plots,[5,5,8,8,5],[1,2,2,1,1],thick=2
xyouts,6.5,1.25,'Keyboard',align=0.5,charsize=3.5


wset,mainwin
device,copy=[0,0,xs,350,0,0,buttonwin]


wait,0.1
cursor,c_x,c_y,/up,/data
;;; Add Selection to Zoo2 ;;;

yscale=(350./ys)

if (c_x ge 5) and (c_x le 8) and (c_y ge 5*yscale) and (c_y le 6*yscale) then begin
zoo2[j].glitch=abs(abs(zoo2[j].glitch)-1)
endif

if (c_x ge 1) and (c_x le 4) and (c_y ge 5*yscale) and (c_y le 6*yscale) then begin
zoo2[j].connected=abs(abs(zoo2[j].connected)-1)
endif

if (c_x ge 5) and (c_x le 8) and (c_y ge 7*yscale) and (c_y le 8*yscale) then begin
zoo2[j].complex=abs(abs(zoo2[j].complex)-1)
endif

if (c_x ge 1) and (c_x le 4) and (c_y ge 7*yscale) and (c_y le 8*yscale) then begin
zoo2[j].good=abs(abs(zoo2[j].good)-1)
endif

if (c_x ge 1) and (c_x le 4) and (c_y ge 3*yscale) and (c_y le 4*yscale) then begin
if (zoo2[j].good+zoo2[j].complex+zoo2[j].connected+zoo2[j].glitch gt 0) then begin
save,zoo2,f=name+'_zoo2.sav'
endif
currentblob=(currentblob-1)>0
direction=-1
j=(j-1)>0
workingonblob=0
buttons=0
endif

if (c_x ge 5) and (c_x le 8) and (c_y ge 3*yscale) and (c_y le 4*yscale) then begin
if (zoo2[j].good+zoo2[j].complex+zoo2[j].connected+zoo2[j].glitch gt 0) then begin
save,zoo2,f=name+'_zoo2.sav'
if zoo1.status[cube[0],cube[1]] ge 0 then begin
zoo1.status[cube[0],cube[1]]=(zoo1.status[cube[0],cube[1]]>currentblob)
save,zoo1,f=name+'_zoo1.sav'
endif
currentblob+=1
direction=1
j+=1
workingonblob=0
buttons=0
endif else begin
print,'You Must Make a Selection Before Moving On'
endelse
endif

if (c_x ge 1) and (c_x le 4) and (c_y ge 1*yscale) and (c_y le 2*yscale) then begin

window,/free,xsize=600,ysize=200,retain=2
exitwin=!d.window
plot,[0,9],[0,3],ymar=[0,0],xmar=[0,0],/nodata

plots,[1,1,4,4,1],[1,2,2,1,1],thick=2
xyouts,2.5,1.25,'New Cube',align=0.5,charsize=3.5

plots,[5,5,8,8,5],[1,2,2,1,1],thick=2
xyouts,6.5,1.25,'Exit BlobZoo',align=0.5,charsize=3.5

exitflag=1
while exitflag eq 1 do begin
cursor,x,y,/up,/data

if (x ge 1) and (x le 4) and (y ge 1) and (y le 2) then begin
workingonblob=0
workingoncube=0
buttons=0
exitflag=0
wdelete,mainwin
wdelete,buttonwin
wdelete,exitwin
wdelete,2
endif

if (x ge 5) and (x le 8) and (y ge 1) and (y le 2) then begin
workingonblob=0
workingoncube=0
buttons=0
exitflag=0
working=0
wdelete,mainwin
wdelete,buttonwin
wdelete,exitwin
wdelete,2
endif
endwhile

endif


if (c_x ge .367*14) and (c_x le (.367*14+300./xs*14.)) and (c_y ge 9.*.65) and (c_y le (9.*.65+300./ys*9)) then begin

ocxs=(((float(ox22)-ox11)/(oy22-oy11))<1)
ocys=(((float(oy22)-oy11)/(ox22-ox11))<1)
if ocxs[0] lt ocys[0] then aspp=ocxs[0] else aspp=1/ocys[0]
window,/free,retain=2,xsize=ocxs[0]*650,ysize=ocys[0]*650
cruisewin=!d.window
display, blobcube[*, *, 0], aspect=aspp,min=cruisemin,max=cruisemax
!mouse.button=0.
while !mouse.button ne 1 do begin
    cursor, x, y, /change, /norm
    v = (x*bcsz[3] < bcsz[3]) > 0.
    display, blobcube[*, *, v], aspect=aspp,min=cruisemin,max=cruisemax
ox3=(ox1-ox11)
ox4=(ox1-ox11)+(ox2-ox1)
oy3=(oy1-oy11)
oy4=(oy1-oy11)+(oy2-oy1)
    plots,[ox3,ox3,ox4,ox4,ox3],[oy3,oy4,oy4,oy3,oy3],thick=5

xyouts,10,12,'V='+string(vels[pig_fit[currentblob].pv[0]+floor(v)]),/device,charsize=3
endwhile
wdelete,cruisewin

endif

if (c_x ge .7*14) and (c_x le (.7*14+300./xs*14.)) and (c_y ge 9.*.65) and (c_y le (9.*.65+300./ys*9)) then begin
cxs=(((float(x22)-x11)/(y22-y11))<1)
cys=(((float(y22)-y11)/(x22-x11))<1)
if cxs[0] lt cys[0] then aspp=cxs[0] else aspp=1/cys[0]
window,/free,retain=2,xsize=cxs[0]*650,ysize=cys[0]*650
cruisewin=!d.window
cruisenoise=sqrt(noise2d[x11:x22,y11:y22]^2*smoothysize)/smoothysize
cruisesz=size(gsblobcube)
cruisecube=gsblobcube/rebin(cruisenoise,cruisesz[1],cruisesz[2],cruisesz[3])
display, gsblobcube[*, *, 0], aspect=aspp,min=gscruisemin,max=gscruisemax
!mouse.button=0.
while !mouse.button ne 1 do begin
    cursor, x, y, /change, /norm
    v = (x*bcsz[3] < bcsz[3]) > 0.
    display, gsblobcube[*, *, v], aspect=aspp,min=gscruisemin,max=gscruisemax
x3=(x1-x11)
x4=(x1-x11)+(x2-x1)
y3=(y1-y11)
y4=(y1-y11)+(y2-y1)
loadct,2,/silent
contour,cruisecube[*,*,v],levels=[4],thick=2,/noerase,/xsty,/ysty,pos=[0.09,0.09,0.98,0.95],color=100
loadct,0,/silent

    plots,[x3,x3,x4,x4,x3],[y3,y4,y4,y3,y3],thick=5
xyouts,10,12,'V='+string(vels[pig_fit[currentblob].pv[0]+floor(v)]),/device,charsize=3



endwhile
wdelete,cruisewin
endif






if (c_x ge .1*14) and (c_x le (.5*14)) and (c_y ge 9.*.45) and (c_y le (9.*.6)) then begin
window,/free,/pixmap,xsize=xs/2,ysize=ys

specwin=!d.window
specmin=min(fits[ox1:ox2,oy1:oy2,pig_fit[currentblob].pv[0]-5:pig_fit[currentblob].pv[1]+5])
specmax=max(fits[ox1:ox2,oy1:oy2,pig_fit[currentblob].pv[0]-5:pig_fit[currentblob].pv[1]+5])
loadct,2,/silent
p_specplot=p_spec2[stmax mod 31,stmax/31,pig_fit[currentblob].pv[0]:pig_fit[currentblob].pv[1]]
plot,vels,p_spec2[stmax mod 31,stmax/31,*],/ysty,/xsty,pos=[0.1*2,0.45,0.5*2,0.6],$
        xran=[vels[(pig_fit[currentblob].pv[0]-30)>0],vels[(pig_fit[currentblob].pv[1]+30)<2047]],yran=[specmin,specmax],charsize=2,font=-1
oplot,p_vels,p_specplot,color=100
wset,mainwin
device,copy=[0,.4*ys,xs/2,.22*ys,0,0.4*ys,specwin]

!mouse.button=0.
while !mouse.button ne 1 do begin
loadct,2,/silent
    cursor, x, y, /change, /norm
specx=floor(((x-.367)*31*xs/300.)>0)<30
specy=floor(((y-.65)*31*ys/300.)>0)<30
p_vels=vels[pig_fit[currentblob].pv[0]:pig_fit[currentblob].pv[1]]
p_specplot=p_spec2[specx,specy,pig_fit[currentblob].pv[0]:pig_fit[currentblob].pv[1]]
wset,specwin
plot,vels,p_spec2[specx,specy,*],/ysty,/xsty,pos=[0.1*2,0.45,0.5*2,0.6],$
        xran=[vels[(pig_fit[currentblob].pv[0]-30)>0],vels[(pig_fit[currentblob].pv[1]+30)<2047]],yran=[specmin,specmax],charsize=2,font=-1
oplot,p_vels,p_specplot,color=100
wset,mainwin
device,copy=[0,0.4*ys,xs/2,ys*0.22,0,.4*ys,specwin]



endwhile
wait,0.1
wset,specwin
plot,vels,p_spec,/ysty,/xsty,pos=[0.1*2,0.45,0.5*2,0.6],$
        xran=[vels[(pig_fit[currentblob].pv[0]-30)>0],vels[(pig_fit[currentblob].pv[1]+30)<2047]],yran=[pspecmin,pspecmax],charsize=2,font=-1
oplot,p_vels,p_spec3,color=100
wset, mainwin
device,copy=[0,0.4*ys,xs/2,ys*0.22,0,.4*ys,specwin]
loadct,0,/silent

endif

if (c_x ge .55*14) and (c_x le (.95*14)) and (c_y ge 9.*.45) and (c_y le (9.*.6)) then begin
window,/free,/pixmap,xsize=xs/2,ysize=ys

specwin=!d.window
specmin=min(spore[x1:x2,y1:y2,pig_fit[currentblob].pv[0]-5:pig_fit[currentblob].pv[1]+5])
specmax=max(spore[x1:x2,y1:y2,pig_fit[currentblob].pv[0]-5:pig_fit[currentblob].pv[1]+5])
loadct,2,/silent
p_specplot=gsspeccube[stmax mod 31,stmax/31,pig_fit[currentblob].pv[0]:pig_fit[currentblob].pv[1]]
plot,vels,gsspeccube[stmax mod 31,stmax/31,*],/ysty,/xsty,pos=[0.05*2,0.45,0.45*2,0.6],$
        xran=[vels[(pig_fit[currentblob].pv[0]-30)>0],vels[(pig_fit[currentblob].pv[1]+30)<2047]],yran=[specmin,specmax],charsize=2,font=-1
oplot,p_vels,p_specplot,color=100
wset,mainwin
device,copy=[0,.4*ys,xs/2,.22*ys,xs/2,0.4*ys,specwin]

!mouse.button=0.
while !mouse.button ne 1 do begin
loadct,2,/silent
    cursor, x, y, /change, /norm
specx=floor(((x-.7)*31*xs/300.)>0)<30
specy=floor(((y-.65)*31*ys/300.)>0)<30
p_vels=vels[pig_fit[currentblob].pv[0]:pig_fit[currentblob].pv[1]]
p_specplot=gsspeccube[specx,specy,pig_fit[currentblob].pv[0]:pig_fit[currentblob].pv[1]]
wset,specwin
plot,vels,gsspeccube[specx,specy,*],/ysty,/xsty,pos=[0.05*2,0.45,0.45*2,0.6],$
	xran=[vels[(pig_fit[currentblob].pv[0]-30)>0],vels[(pig_fit[currentblob].pv[1]+30)<2047]],yran=[specmin,specmax],charsize=2,font=-1
oplot,p_vels,p_specplot,color=100
wset,mainwin
device,copy=[0,0.4*ys,xs/2,ys*0.22,xs/2,.4*ys,specwin]


endwhile
wait,0.1
wset,specwin
plot,vels,gsspecint,/ysty,/xsty,pos=[0.05*2,0.45,0.45*2,0.6],$
        xran=[vels[(pig_fit[currentblob].pv[0]-30)>0],vels[(pig_fit[currentblob].pv[1]+30)<2047]],yran=[gsspecmin,gsspecmax],charsize=2,font=-1
oplot,p_vels,gsspecint2,color=100
wset, mainwin
device,copy=[0,0.4*ys,xs/2,ys*0.22,xs/2,.4*ys,specwin]
loadct,0,/silent

endif










if (c_x ge 5) and (c_x le 8) and (c_y ge 1*yscale) and (c_y le 2*yscale) then begin
print,'Switching to Keyboard Control'
keyboard=1
mouse=0
endif
endif


if keyboard eq 1 then begin
wset,buttonwin
plot,[0,9],[0,9],/xsty,/ysty,/nodata,ticklen=0,xmarg=[0,0],ymarg=[0,0.1],thick=3

if zoo2[j].good eq 1 then polyfill,[1,1,4,4,1],[7,8,8,7,7],color=100,thick=2
plots,[1,1,4,4,1],[7,8,8,7,7],thick=2
xyouts,2.5,7.25,'Good (T)',align=0.5,charsize=3.5

if zoo2[j].complex eq 1 then polyfill,[5,5,8,8,5],[7,8,8,7,7],color=100,thick=2
plots,[5,5,8,8,5],[7,8,8,7,7],thick=2
xyouts,6.5,7.25,'Complex (Y)',align=0.5,charsize=3.5

if zoo2[j].connected eq 1 then polyfill,[1,1,4,4,1],[5,6,6,5,5],color=100,thick=2
plots,[1,1,4,4,1],[5,6,6,5,5],thick=2
xyouts,2.5,5.25,'Connected (G)',align=0.5,charsize=3.5

if zoo2[j].glitch eq 1 then polyfill,[5,5,8,8,5],[5,6,6,5,5],color=100,thick=2
plots,[5,5,8,8,5],[5,6,6,5,5],thick=2
xyouts,6.5,5.25,'Glitch (H)',align=0.5,charsize=3.5

plots,[1,1,4,4,1],[3,4,4,3,3],thick=2
xyouts,2.5,3.25,'Previous Blob (B)',align=0.5,charsize=3.5

plots,[5,5,8,8,5],[3,4,4,3,3],thick=2
xyouts,6.5,3.25,'Next Blob (N)',align=0.5,charsize=3.5

plots,[1,1,4,4,1],[1,2,2,1,1],thick=2
xyouts,2.5,1.25,'Exit Cube (X)',align=0.5,charsize=3.5

plots,[5,5,8,8,5],[1,2,2,1,1],thick=2
xyouts,6.5,1.25,'Mouse (M)',align=0.5,charsize=3.5

wset,mainwin
device,copy=[0,0,700,350,0,0,buttonwin]

k_in=''
read,k_in,prompt='>'
k_in=strlowcase(k_in)
if k_in eq 't' then begin
zoo2[j].good=abs(abs(zoo2[j].good)-1)
endif else begin

if k_in eq 'y' then begin
zoo2[j].complex=abs(abs(zoo2[j].complex)-1)
endif else begin

if k_in eq 'g' then begin
zoo2[j].connected=abs(abs(zoo2[j].connected)-1)
endif else begin

if k_in eq 'h' then begin
zoo2[j].glitch=abs(abs(zoo2[j].glitch)-1)
endif else begin

if k_in eq 'b' then begin
if (zoo2[j].good+zoo2[j].complex+zoo2[j].connected+zoo2[j].glitch gt 0) then begin
save,zoo2,f=name+'_zoo2.sav'
endif
currentblob=(currentblob-1)>0
direction=-1
j=(j-1)>0
workingonblob=0
buttons=0
endif else begin

if k_in eq 'n' then begin
if (zoo2[j].good+zoo2[j].complex+zoo2[j].connected+zoo2[j].glitch gt 0) then begin
save,zoo2,f=name+'_zoo2.sav'
if zoo1.status[cube[0],cube[1]] ge 0 then begin
zoo1.status[cube[0],cube[1]]=(zoo1.status[cube[0],cube[1]]>currentblob)
save,zoo1,f=name+'_zoo1.sav'
endif
currentblob+=1
direction=1
j+=1
workingonblob=0
buttons=0
endif else begin
print,'You Must Make a Selection Before Moving On'
endelse
endif else begin

if k_in eq 'x' then begin
window,/free,xsize=600,ysize=200,retain=2
exitwin=!d.window
plot,[0,9],[0,3],ymar=[0,0],xmar=[0,0],/nodata

plots,[1,1,4,4,1],[1,2,2,1,1],thick=2
xyouts,2.5,1.25,'New Cube (C)',align=0.5,charsize=3.5

plots,[5,5,8,8,5],[1,2,2,1,1],thick=2
xyouts,6.5,1.25,'Exit BlobZoo (X)',align=0.5,charsize=3.5

exitflag=1
while exitflag eq 1 do begin
x_in=''
read,x_in,prompt='>'
x_in=strlowcase(x_in)

if x_in eq 'c' then begin
workingonblob=0
workingoncube=0
buttons=0
exitflag=0
wdelete,mainwin
wdelete,buttonwin
wdelete,exitwin
wdelete,2
endif else begin
if x_in eq 'x' then begin
workingonblob=0
workingoncube=0
buttons=0
exitflag=0
working=0
wdelete,mainwin
wdelete,buttonwin
wdelete,exitwin
wdelete,2
endif else begin
print,'Incorrect Input, Try again'
endelse
endelse
endwhile

endif else begin

if k_in eq 'm' then begin
keyboard=0
mouse=1
endif else begin

print,'Invalid Input, Refer to Window for options'

endelse
endelse
endelse
endelse
endelse
endelse
endelse
endelse

endif










endwhile  ;buttons
endwhile  ;workingonblob

if currentblob eq n_blobs then begin

zoo1.status[cube[0],cube[1]]=-1
save,zoo1,f=name+'_zoo1.sav'


window,/free,xsize=600,ysize=200,retain=2
exitwin=!d.window
plot,[0,9],[0,3],ymar=[0,0],xmar=[0,0],/nodata

plots,[1,1,4,4,1],[1,2,2,1,1],thick=2
xyouts,2.5,1.25,'New Cube',align=0.5,charsize=3.5

plots,[5,5,8,8,5],[1,2,2,1,1],thick=2
xyouts,6.5,1.25,'Exit BlobZoo',align=0.5,charsize=3.5

exitflag=1
while exitflag eq 1 do begin
cursor,x,y,/up,/data

if (x ge 1) and (x le 4) and (y ge 1) and (y le 2) then begin
workingoncube=0
buttons=0
exitflag=0
wdelete,exitwin
wdelete,2
endif

if (x ge 5) and (x le 8) and (y ge 1) and (y le 2) then begin
workingoncube=0
exitflag=0
working=0
wdelete,exitwin
wdelete,2
endif
endwhile




endif






endwhile

; Delete Windows ;






;working=0
endwhile

end

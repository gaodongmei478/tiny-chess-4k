F='abcdefgh'
import json,os,urllib.request as U
ENV=os.environ['MAKEFUN_ENV_URL']
V=[0,100,320,330,500,900,0]
PST=[[0]*64 for _ in range(7)]
for i in range(64):
 r=i//8;f=i%8;c=min(f,7-f)+min(r,7-r)
 PST[1][i]=r*10+(10 if f in(3,4) and r>2 else 0)
 PST[2][i]=6*c-20
 PST[3][i]=4*c-8
 PST[4][i]=(r==6)*12
 PST[5][i]=2*c-6
 PST[6][i]=20 if r==0 else -15*r
PS=[i for i in range(21,99) if i%10 not in(0,9)]
CM=[0]*120
for q in((25,3),(21,2),(28,1),(95,12),(91,8),(98,4)):CM[q[0]]=q[1]
N8=(8,12,19,21,-8,-12,-19,-21);K8=(1,9,10,11,-1,-9,-10,-11)
SD=((9,11,-9,-11),(1,10,-1,-10),(1,9,10,11,-1,-9,-10,-11))
B=[7]*120;S=1;C=0;EP=-1;N=0;LIM=10**9;BUD=60000;MATE=99999
def gen(z=0):
 L=[]
 for f in PS:
  p=B[f]
  if p*S<1:continue
  a=abs(p)
  if a==1:
   t=f+10*S
   if not B[t]:
    if t//10 in(2,9):L.append(f|t<<7|81920)
    elif not z:
     L.append(f|t<<7)
     if f//10 in(3,8) and not B[f+20*S]:L.append(f|f+20*S<<7)
   for d in(9,11):
    t=f+d*S;b=B[t]
    if -7<b*S<0 or(t==EP and not b):
     if t//10 in(2,9):L.append(f|t<<7|81920)
     else:L.append(f|t<<7)
  elif a in(2,6):
   for d in(N8,K8)[a==6]:
    t=f+d;b=B[t]
    if -7<b*S<0 or(not z and not b):L.append(f|t<<7)
   if a==6 and not z:
    for k,g,r in((0,25,28),(1,25,21),(2,95,98),(3,95,91)):
     if C>>k&1 and f==g and not any(B[x] for x in range(min(g,r)+1,max(g,r))):L.append(g|(g+2*(1 if r>g else -1))<<7)
  else:
   for d in SD[a-3]:
    t=f+d;b=B[t]
    while not b:
     if not z:L.append(f|t<<7)
     t+=d;b=B[t]
    if -7<b*S<0:L.append(f|t<<7)
 return L
def MK(m):
 global C,EP
 f=m&127;t=m>>7&127;q=m>>14
 p=B[f];c=B[t];s=t;o=C;e=EP;a=abs(p)
 if a==1:
  if t==EP and not c:s=t-10*S;c=B[s];B[s]=0
 elif a==6 and abs(t-f)==2:
  if t>f:B[f+3]=0;B[f+1]=S*4
  else:B[f-4]=0;B[f-1]=S*4
 if q:p=S*q
 B[f]=0;B[t]=p
 C=o&~CM[f]&~CM[t];EP=-1
 if a==1 and abs(t-f)==20:EP=(f+t)//2
 return c,s,o,e
def UN(m,u):
 global C,EP
 c,s,o,e=u
 f=m&127;t=m>>7&127;q=m>>14
 p=B[t]
 if p==6*S and abs(t-f)==2:
  if t>f:B[f+1]=0;B[f+3]=S*4
  else:B[f-1]=0;B[f-4]=S*4
 B[f]=S if q else p
 B[t]=0;B[s]=c
 C=o;EP=e
def EV():
 sc=0;kp=[0,0]
 for i in PS:
  p=B[i]
  if p:
   a=abs(p);f=i%10-1;r=(i//10-2) if p>0 else 9-i//10
   if a==6:kp[p<0]=r*8+f
   else:
    v=V[a]+PST[a][r*8+f]
    sc+=v if p>0 else -v
 return (sc+PST[6][kp[0]]-PST[6][kp[1]])*S
def ORD(L):
 def k(m):
  t=m>>7&127;q=m>>14;c=B[t];s=0
  if -7<c*S<0:s=100+10*V[c]-V[abs(B[m&127])]
  if q:s+=2e5+V[q]
  return -s
 L.sort(key=k)
def SE(d,a,b):
 global N,S
 N+=1
 if N>LIM:1/0
 best=-MATE
 if d<1:
  best=EV()
  if best>=b:return best
  if best>a:a=best
 L=gen(0 if d>0 else 1);ORD(L)
 for m in L:
  if abs(B[m>>7&127])==6:return MATE
  u=MK(m);S=-S
  v=-SE(d-1,-b,-a)
  S=-S;UN(m,u)
  if v>best:best=v
  if v>a:a=v
  if a>=b:return best
 return best
def SET(fen):
 global B,S,C,EP,N
 a=fen.split();B=[7]*120
 for i in PS:B[i]=0
 r=9
 for w in a[0].split('/'):
  f=1
  for c in w:
   if c<'9':f+=int(c);continue
   p=' PNBRQK'.index(c.upper());p=p if c<'a' else -p
   B[r*10+f]=p
   f+=1
  r-=1
 S=1 if a[1]=='w' else -1
 C=sum((c in a[2])<<k for k,c in enumerate('KQkq'))
 EP=-1 if a[3]=='-' else int(a[3][1])*10+F.index(a[3][0])+11
 N=0
def PV(s):
 f=(int(s[1])+1)*10+F.index(s[0])+1
 t=(int(s[3])+1)*10+F.index(s[2])+1
 return f|t<<7|(' pnbrq'.index(s[4])<<14 if len(s)>4 else 0)
def CV(m):
 f=m&127;t=m>>7&127;q=m>>14
 return F[f%10-1]+str(f//10-1)+F[t%10-1]+str(t//10-1)+(' pnbrq'[q] if q else '')
def GO(fen,legal):
 global LIM,S
 SET(fen);L=[PV(x) for x in legal];ORD(L);best=L[0];LIM=10**9
 for d in range(1,64):
  try:
   sc=[];a=-MATE
   for m in L:
    u=MK(m);S=-S
    v=-SE(d-1,-MATE,-a)
    S=-S;UN(m,u)
    sc.append((v,m))
    if v>a:a=v
  except:break
  sc.sort(key=lambda x:-x[0]);L=[m for v,m in sc];best=L[0]
  if sc[0][0]>MATE-100:break
  LIM=BUD
 return CV(best)
def P(p,b):
 r=U.Request(ENV+p,json.dumps(b).encode(),{'content-type':'application/json'})
 return json.load(U.urlopen(r))
g=P('/game',{})
while g['status']!='done':
 while g['status']=='your_move':
  g=P('/move',{'game':g['game'],'move':GO(g['fen'],g['legal_moves'])})
 g=P('/game',{})

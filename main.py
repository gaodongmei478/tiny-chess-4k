F='abcdefgh'
import json,os,urllib.request as U
ENV=os.environ['MAKEFUN_ENV_URL']
V=[0,100,320,330,500,900,0]
PST=[[0]*64 for _ in range(7)]
for i in range(64):
 r=i//8;f=i%8;c=min(f,7-f)+min(r,7-r)
 PST[1][i]=[0,0,3,8,15,25,45,0][r]+(8 if f in(3,4) and r>2 else 0)
 PST[2][i]=6*c-20
 PST[3][i]=4*c-8+(8 if r else 0)
 PST[4][i]=(r==6)*12
 PST[5][i]=2*c-6
 PST[6][i]=[10,0,15,0,0,-10,40,10][f] if r==0 else -20*r
PS=[i for i in range(21,99) if i%10 not in(0,9)]
N8=(8,12,19,21,-8,-12,-19,-21);K8=(1,9,10,11,-1,-9,-10,-11)
SD=((9,11,-9,-11),(1,10,-1,-10),(1,9,10,11,-1,-9,-10,-11))
B=[7]*120;S=1;K=[25,95];N=0;BUD=100000;LIM=BUD;MATE=99999
def A(t,s):
 if B[t-11*s]==s or B[t-9*s]==s:return 1
 for d in N8:
  if B[t+d]==2*s:return 1
 for d in K8:
  if B[t+d]==6*s:return 1
 for d in SD[0]:
  x=t+d
  while not B[x]:x+=d
  if B[x]in(3*s,5*s):return 1
 for d in SD[1]:
  x=t+d
  while not B[x]:x+=d
  if B[x]in(4*s,5*s):return 1
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
    if -7<b*S<0:
     if t//10 in(2,9):L.append(f|t<<7|81920)
     else:L.append(f|t<<7)
  elif a in(2,6):
   for d in(N8,K8)[a==6]:
    t=f+d;b=B[t]
    if -7<b*S<0 or(not z and not b):L.append(f|t<<7)
  else:
   for d in SD[a-3]:
    t=f+d;b=B[t]
    while not b:
     if not z:L.append(f|t<<7)
     t+=d;b=B[t]
    if -7<b*S<0:L.append(f|t<<7)
 return L
def MK(m):
 f=m&127;t=m>>7&127;q=m>>14
 p=B[f];c=B[t];s=t;a=abs(p)
 if a==6:
  if abs(t-f)==2:
   if t>f:B[f+3]=0;B[f+1]=S*4
   else:B[f-4]=0;B[f-1]=S*4
  K[S<0]=t
 if q:p=S*q
 B[f]=0;B[t]=p
 return c,s
def UN(m,u):
 c,s=u
 f=m&127;t=m>>7&127;q=m>>14
 p=B[t]
 if p==6*S:
  K[S<0]=f
  if abs(t-f)==2:
   if t>f:B[f+1]=0;B[f+3]=S*4
   else:B[f-1]=0;B[f-4]=S*4
 B[f]=S if q else p
 B[t]=0;B[s]=c
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
  if abs(c)==6:return -1e9
  if -7<c*S<0:s=100+10*V[abs(c)]-V[abs(B[m&127])]
  if q:s+=2e5+V[q]
  return -s
 L.sort(key=k)
def SE(d,a,b):
 global N,S
 N+=1
 if N>LIM:1/0
 ck=d<1 and A(K[S<0],-S);best=-MATE
 L=gen(0 if d>0 or ck else 1)
 if d<1:
  if not ck:
   for m in L:
    if abs(B[m>>7&127])==6:return MATE
   best=EV()
   if best>=b:return best
   if best>a:a=best
 ORD(L)
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
 global B,S,K,N
 a=fen.split();B=[7]*120
 for i in PS:B[i]=0
 r=9
 for w in a[0].split('/'):
  f=1
  for c in w:
   if c<'9':f+=int(c);continue
   p=' PNBRQK'.index(c.upper());p=p if c<'a' else -p
   B[r*10+f]=p
   if abs(p)==6:K[p<0]=r*10+f
   f+=1
  r-=1
 S=1 if a[1]=='w' else -1
 N=0
def CV(m):
 f=m&127;t=m>>7&127;q=m>>14
 return F[f%10-1]+str(f//10-1)+F[t%10-1]+str(t//10-1)+(' pnbrq'[q] if q else '')
def GO(fen,legal):
 global S
 SET(fen);x=set(legal);L=[m for m in gen() if CV(m) in x]
 for u,m in(('e1g1',3481),('e1c1',2969),('e8g8',12511),('e8c8',11999)):
  if u in x:L.append(m)
 if not L:return legal[0]
 ORD(L);best=L[0]
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
 return CV(best)
def P(p,b):
 r=U.Request(ENV+p,json.dumps(b).encode(),{'content-type':'application/json'})
 return json.load(U.urlopen(r))
g=P('/game',{})
while g['status']!='done':
 while g['status']=='your_move':
  g=P('/move',{'game':g['game'],'move':GO(g['fen'],g['legal_moves'])})
 g=P('/game',{})

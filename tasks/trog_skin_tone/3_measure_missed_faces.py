# Measure face blobs from the full-image parse that no accepted person box covered.
import json, os, numpy as np, torch
from PIL import Image
from scipy import ndimage
from skimage.color import rgb2lab
from sklearn.cluster import KMeans
from transformers import SegformerImageProcessor, AutoModelForSemanticSegmentation
S=os.environ["S"]; DET=f"{S}/trog/det"
proc=SegformerImageProcessor.from_pretrained("mattmdjaga/segformer_b2_clothes")
net=AutoModelForSemanticSegmentation.from_pretrained("mattmdjaga/segformer_b2_clothes").eval()
missed=[m for m in json.load(open(f"{S}/trog/missed_faces.json")) if m["px"]>=300]
rows=json.load(open(f"{S}/trog/skin2.json")); rows=[r for r in rows if r.get("source")!="full"]
for st in sorted({m["image"] for m in missed}):
    im=Image.open(f"{DET}/{st}.png").convert("RGB")
    with torch.no_grad(): lg=net(**proc(images=im,return_tensors="pt")).logits
    seg=torch.nn.functional.interpolate(lg,size=im.size[::-1],mode="bilinear").argmax(1)[0].numpy()
    lbl,n=ndimage.label(seg==11); arr=np.asarray(im)
    for j,m in enumerate([m for m in missed if m["image"]==st]):
        x0,y0,x1,y1=m["bbox"]; c=np.bincount(lbl[y0:y1+1,x0:x1+1].ravel(),minlength=n+1); c[0]=0; blob=lbl==c.argmax()
        ys,xs=np.nonzero(blob); rgb=arr[ys,xs]; lab=rgb2lab(rgb[None]/255.0)[0]
        km=KMeans(3,n_init=4,random_state=0).fit(lab); sel=km.labels_==np.bincount(km.labels_).argmax()
        L,a,b=np.median(lab[sel],axis=0)
        rows.append(dict(image=st,person=100+j,source="full",has_face=True,face_px=int(blob.sum()),
            L=round(float(L),1),a=round(float(a),1),b=round(float(b),1),ITA=round(float(np.degrees(np.arctan2(L-50,b))),1),
            skin_share=round(float(sel.mean()),2),rgb=[int(v) for v in np.median(rgb[sel],axis=0)],face_bbox=m["bbox"]))
json.dump(rows,open(f"{S}/trog/skin2.json","w"),indent=0)
print(len(rows), sum(r["has_face"] for r in rows))

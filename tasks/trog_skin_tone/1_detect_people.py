# Detect people in TROG images with COCO Mask R-CNN (images downscaled to <=900px).
# Saves person masks plus all other detections (for dropping animal look-alikes).
import glob, os, json, numpy as np, torch
from PIL import Image
from torchvision.models.detection import maskrcnn_resnet50_fpn_v2, MaskRCNN_ResNet50_FPN_V2_Weights
D="/Users/mcfrank/Projects/levante/levante-offline-spike/pack-builder/cache/visual/trog"
S=os.environ["S"]; OUT=S+"/trog/det"; os.makedirs(OUT,exist_ok=True)
w=MaskRCNN_ResNet50_FPN_V2_Weights.DEFAULT; m=maskrcnn_resnet50_fpn_v2(weights=w).eval()
cats=w.meta["categories"]
stems={}
for f in sorted(glob.glob(D+"/*")):
    b=os.path.basename(f); st,ext=os.path.splitext(b)
    if not st.split('-')[0].isdigit(): continue
    if st not in stems or ext==".webp": stems[st]=f
res={}
for st,f in stems.items():
    im=Image.open(f).convert("RGBA"); bg=Image.new("RGBA",im.size,"white")
    im=Image.alpha_composite(bg,im).convert("RGB"); im.thumbnail((900,900)); im.save(f"{OUT}/{st}.png")
    x=torch.from_numpy(np.asarray(im)).permute(2,0,1).float()/255
    with torch.no_grad(): o=m([x])[0]
    keep=(o["labels"]==1)&(o["scores"]>0.5)
    np.savez_compressed(f"{OUT}/{st}.npz",masks=(o["masks"][keep,0]>0.5).numpy(),
        boxes=o["boxes"][keep].numpy(),scores=o["scores"][keep].numpy())
    other=[dict(label=cats[int(l)],score=round(float(s),3),box=[round(float(v)) for v in b])
           for l,s,b in zip(o["labels"],o["scores"],o["boxes"]) if s>0.3 and int(l)!=1]
    res[st]={"file":f,"n_person":int(keep.sum()),"scores":[round(float(s),3) for s in o["scores"][keep]],"other":other}
json.dump(res,open(f"{S}/trog/detections.json","w"),indent=0)
print(len(res), sum(v["n_person"]>0 for v in res.values()))

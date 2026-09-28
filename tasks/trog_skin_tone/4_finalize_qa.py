# Final per-person and per-image skin tables, after visual QA of every measurement.
import json, os, numpy as np, torch, csv
from PIL import Image
from skimage.color import rgb2lab
from sklearn.cluster import KMeans
from transformers import SegformerImageProcessor, AutoModelForSemanticSegmentation
S = os.environ["S"]; DET = f"{S}/trog/det"
rows = json.load(open(f"{S}/trog/skin2.json"))

# Visual QA: face measurements that are not people (animal faces, objects, clothing).
NOT_PERSON = {("12-black-horse",100), ("73-brown-ball-brown-dog",100), ("73-brown-dog-white-ball",100),
  ("21-horse-jump-box",100), ("93-turtle-behind-duck",100), ("93-duck-turtle-on-bridge",100),
  ("93-duck-behind-turtle",100), ("26-girl-faces-cow",100), ("20-dog-sitting",100), ("20-dog-sitting",101),
  ("98-eat-and-swing",100), ("98-eat-and-swing",101), ("98-eat-brown-banana",100), ("98-eat-banana",100),
  ("98-swing-not-eat",0), ("98-eat-not-swing",100), ("98-not-eat-not-swing",100), ("98-look-brown-banana",100),
  ("45-horse-chase-girl",100), ("12-brown-horse",100), ("12-brown-horse",101), ("12-yellow-dog",1),
  ("9-short3",100), ("15-dog-sitting",0), ("15-dog-sitting",100), ("30-sheep-chases-man",0),
  ("42-box-bigger-cup",100), ("42-box-bigger-cup",101), ("42-box-bigger-cup",102), ("49-cup-in-box",100),
  ("41-fork-spoon",100), ("32-girl-pushes-woman",100), ("84-keys-under-couch-no-pillows",1), ("41-knife-spoon",100), ("4-elephant",100), ("4-elephant",101)}
# Visual QA: real people with no visible face (back view / face down) -> measure arm & leg skin.
LIMB_ONLY = {"33-girl-behind-chair", "36-woman-on-bench", "39-boys-pick-apples", "8-dancing",
             "84-keys-under-couch-no-pillows", "84-keys-under-couch-pillows"}

proc = SegformerImageProcessor.from_pretrained("mattmdjaga/segformer_b2_clothes")
net = AutoModelForSemanticSegmentation.from_pretrained("mattmdjaga/segformer_b2_clothes").eval()
def skin_of(rgb):
    lab = rgb2lab(rgb[None] / 255.0)[0]
    km = KMeans(3, n_init=4, random_state=0).fit(lab); sel = km.labels_ == np.bincount(km.labels_).argmax()
    L, a, b = np.median(lab[sel], axis=0)
    return dict(L=round(float(L),1), a=round(float(a),1), b=round(float(b),1),
                ITA=round(float(np.degrees(np.arctan2(L-50, b))),1), rgb=[int(v) for v in np.median(rgb[sel], axis=0)])

people = []
for r in rows:
    if (r["image"], r["person"]) in NOT_PERSON: continue
    if r["has_face"]:
        people.append(r | dict(basis="face"))
    elif r["image"] in LIMB_ONLY:
        im = Image.open(f"{DET}/{r['image']}.png").convert("RGB"); crop = im.crop(r["box"])
        f = 512 / min(crop.size); up = crop.resize((round(crop.width*f), round(crop.height*f)))
        with torch.no_grad(): lg = net(**proc(images=up, return_tensors="pt")).logits
        seg = torch.nn.functional.interpolate(lg, size=up.size[::-1], mode="bilinear").argmax(1)[0].numpy()
        limb = np.isin(seg, [12, 13, 14, 15])
        if limb.sum() < 150: continue
        people.append(r | dict(basis="limbs", has_face=False) | skin_of(np.asarray(up)[limb]))

# De-duplicate: the same face measured by both passes, or by two overlapping person boxes.
def iou(a, b):  # overlap coefficient: intersection / smaller box (catches partial-face duplicates)
    ix = max(0, min(a[2], b[2]) - max(a[0], b[0])); iy = max(0, min(a[3], b[3]) - max(a[1], b[1])); I = ix * iy
    return I / (min((a[2]-a[0])*(a[3]-a[1]), (b[2]-b[0])*(b[3]-b[1])) + 1e-9)
kept = []
for p in sorted(people, key=lambda p: (p["image"], p["person"])):
    if p["basis"] == "face" and any(q["image"] == p["image"] and q["basis"] == "face"
                                    and iou(q["face_bbox"], p["face_bbox"]) > 0.5 for q in kept):
        continue
    kept.append(p)

with open(f"{S}/trog/person_skin.csv", "w", newline="") as fh:
    w = csv.writer(fh, lineterminator="\n"); w.writerow(["image","person","basis","L","a","b","ITA","r","g","bl"])
    for p in kept: w.writerow([p["image"], p["person"], p["basis"], p["L"], p["a"], p["b"], p["ITA"], *p["rgb"]])
imgs = sorted({os.path.basename(f)[:-4] for f in os.listdir(DET) if f.endswith(".npz")})
with open(f"{S}/trog/image_skin.csv", "w", newline="") as fh:
    w = csv.writer(fh, lineterminator="\n"); w.writerow(["image","n_person","ita_mean","ita_min","ita_max"])
    for st in imgs:
        v = [p["ITA"] for p in kept if p["image"] == st]
        w.writerow([st, len(v)] + ([round(np.mean(v),1), min(v), max(v)] if v else ["", "", ""]))
print(len(people), "->", len(kept), "people;", sum(p["basis"]=="limbs" for p in kept), "limb-based;",
      len({p["image"] for p in kept}), "images with people of", len(imgs))

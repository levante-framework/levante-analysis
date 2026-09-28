# Skin tone per depicted person in the TROG images.
#  1. Mask R-CNN person boxes (detect.py), partial duplicates removed.
#  2. Human-parsing SegFormer (ATR labels) on each upscaled person crop -> "Face" pixels.
#     No face => not a person (animals, trees) or a back view; dropped and listed for review.
#  3. Face pixels -> k-means(3) in CIELAB, keep the largest cluster (drops eyes, mouth, blush),
#     median L*, a*, b* -> Individual Typology Angle ITA = atan((L*-50)/b*) in degrees.
# Also parses the whole image to flag faces not covered by any person box (missed people).
import glob, os, json, numpy as np, torch
from PIL import Image
from scipy import ndimage
from skimage.color import rgb2lab
from sklearn.cluster import KMeans
from transformers import SegformerImageProcessor, AutoModelForSemanticSegmentation
S = os.environ["S"]; DET = f"{S}/trog/det"; FACE = 11
proc = SegformerImageProcessor.from_pretrained("mattmdjaga/segformer_b2_clothes")
net = AutoModelForSemanticSegmentation.from_pretrained("mattmdjaga/segformer_b2_clothes").eval()

def parse(im):
    with torch.no_grad(): lg = net(**proc(images=im, return_tensors="pt")).logits
    return torch.nn.functional.interpolate(lg, size=im.size[::-1], mode="bilinear").argmax(1)[0].numpy()

rows, missed = [], []
for npz in sorted(glob.glob(f"{DET}/*.npz")):
    st = os.path.basename(npz)[:-4]
    im = Image.open(f"{DET}/{st}.png").convert("RGB"); W, H = im.size
    z = np.load(npz); covered = np.zeros((H, W), bool)
    for i, (m, box, sc) in enumerate(zip(z["masks"], z["boxes"], z["scores"])):
        if any(j < i and (m & z["masks"][j]).sum() > 0.6 * m.sum() for j in range(i)): continue
        x0, y0, x1, y1 = box; pw, ph = (x1 - x0) * .1, (y1 - y0) * .1
        b = [int(max(0, x0 - pw)), int(max(0, y0 - ph)), int(min(W, x1 + pw)), int(min(H, y1 + ph))]
        crop = im.crop(b); f = 512 / min(crop.size); up = crop.resize((round(crop.width * f), round(crop.height * f)))
        seg = parse(up)
        face = seg == FACE
        # keep the largest face blob (a crop can clip a neighbour's face)
        lbl, n = ndimage.label(face)
        if n: face = lbl == (np.argmax(np.bincount(lbl.ravel())[1:]) + 1)
        r = dict(image=st, person=i, det_score=round(float(sc), 3), box=[int(v) for v in box],
                 face_px=int(face.sum()), face_frac=round(float(face.sum() / face.size), 4))
        if face.sum() < 150:
            rows.append(r | dict(has_face=False)); continue
        ys, xs = np.nonzero(face)
        cy, cx = ys / f + b[1], xs / f + b[0]; covered[np.clip(cy.astype(int), 0, H-1), np.clip(cx.astype(int), 0, W-1)] = True
        rgb = np.asarray(up)[ys, xs]; lab = rgb2lab(rgb[None] / 255.0)[0]
        km = KMeans(3, n_init=4, random_state=0).fit(lab)
        k = np.bincount(km.labels_).argmax(); sel = km.labels_ == k
        L, a, bb = np.median(lab[sel], axis=0)
        rows.append(r | dict(has_face=True, L=round(float(L), 1), a=round(float(a), 1), b=round(float(bb), 1),
                             ITA=round(float(np.degrees(np.arctan2(L - 50, bb))), 1),
                             skin_share=round(float(sel.mean()), 2),
                             rgb=[int(v) for v in np.median(rgb[sel], axis=0)],
                             face_bbox=[int(cx.min()), int(cy.min()), int(cx.max()), int(cy.max())]))
    # faces in the full image not covered by any accepted person
    seg = parse(im); lbl, n = ndimage.label(seg == FACE)
    for c in range(1, n + 1):
        blob = lbl == c
        if blob.sum() >= 60 and (blob & ndimage.binary_dilation(covered, iterations=6)).sum() < 0.3 * blob.sum():
            ys, xs = np.nonzero(blob); missed.append(dict(image=st, px=int(blob.sum()), bbox=[int(xs.min()), int(ys.min()), int(xs.max()), int(ys.max())]))
json.dump(rows, open(f"{S}/trog/skin2.json", "w"), indent=0)
json.dump(missed, open(f"{S}/trog/missed_faces.json", "w"), indent=0)
print(len(rows), sum(r["has_face"] for r in rows), "missed-face blobs:", len(missed))

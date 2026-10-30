import json, os, sys, numpy as np
from PIL import Image, ImageDraw
S = os.environ["S"]; DET = f"{S}/trog/det"
rows = json.load(open(f"{S}/trog/skin2.json")); mode = sys.argv[1]
if mode == "face":
    rows = sorted([r for r in rows if r["has_face"]], key=lambda r: r["ITA"])
    lo, hi = int(sys.argv[2]), int(sys.argv[3]); rows = rows[lo:hi]
    T = 96; cols = 10; cw = T + 50; rh = T + 26
    sheet = Image.new("RGB", (cols * cw, ((len(rows) + cols - 1) // cols) * rh), "white"); dr = ImageDraw.Draw(sheet)
    for i, r in enumerate(rows):
        im = Image.open(f"{DET}/{r['image']}.png").convert("RGB")
        x0, y0, x1, y1 = r["face_bbox"]; p = max(x1 - x0, y1 - y0) * 0.6
        c = im.crop((int(x0 - p), int(y0 - p), int(x1 + p), int(y1 + p))).resize((T, T))
        x = (i % cols) * cw; y = (i // cols) * rh
        sheet.paste(c, (x, y)); dr.rectangle([x + T + 2, y, x + T + 46, y + 44], fill=tuple(r["rgb"]))
        dr.text((x + T + 4, y + 48), f"{r['ITA']:.0f}", fill="black")
        dr.text((x, y + T + 2), f"{r['image'][:20]}", fill="black"); dr.text((x, y + T + 12), f"p{r['person']}", fill="grey")
else:  # detections dropped for having no face
    rows = [r for r in rows if not r["has_face"]]
    T = 120; cols = 10; rh = T + 14
    sheet = Image.new("RGB", (cols * T, ((len(rows) + cols - 1) // cols) * rh), "white"); dr = ImageDraw.Draw(sheet)
    for i, r in enumerate(rows):
        c = Image.open(f"{DET}/{r['image']}.png").convert("RGB").crop(r["box"]); c.thumbnail((T, T))
        x = (i % cols) * T; y = (i // cols) * rh; sheet.paste(c, (x, y)); dr.text((x, y + T), r["image"][:22], fill="black")
sheet.save(sys.argv[-1]); print(len(rows))

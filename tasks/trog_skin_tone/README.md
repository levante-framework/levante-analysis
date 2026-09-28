# Skin tone of the people depicted in the TROG (Sentence Understanding) art

Stimulus metadata for `tasks/trog_skin_tone.qmd`. No child data here.

**Outputs**

- `trog_person_skin.csv`: one row per depicted person (313 people in 229 of 442 images):
  median CIELAB L\*, a\*, b\* of their skin, the Individual Typology Angle
  `ITA = atan((L* - 50) / b*)` in degrees (higher = lighter), the median sRGB swatch,
  and `basis` (`face`, or `limbs` for 6 people shown from behind or face-down).
- `trog_image_skin.csv`: one row per image: `n_person` and the mean / min / max ITA.

**Images**: the production bucket `levante-assets-prod/visual/trog/` as cached on
2026-09-04 by the offline launcher's pack builder
(`levante-offline-spike/pack-builder/cache/visual/trog/`). Item → image mapping comes from
`corpus/trog/trog-item-bank.csv` (same cache); it agrees with children's recorded responses
on 99.4–100% of trials per dataset.

**Pipeline** (python venv from `requirements.txt`; set `S` to a work directory; the scripts
write to `$S/trog/`):

1. `1_detect_people.py`: COCO Mask R-CNN (torchvision v2 weights) finds people; images
   downscaled to ≤900 px.
2. `2_measure_faces.py`: SegFormer human parsing (`mattmdjaga/segformer_b2_clothes`,
   ATR labels) on each upscaled person crop gives the **Face** pixels. Animals and objects
   have no face and drop out. Face pixels are clustered with k-means (k = 3) in CIELAB, and
   the largest cluster (which drops eyes, mouth, and blush) gives the median colour. Faces
   that no person box covers are listed for step 3.
3. `3_measure_missed_faces.py`: measures those uncovered face blobs from the full-image
   parse.
4. `4_finalize_qa.py`: applies the **visual QA** (every measurement was checked by eye on
   contact sheets made with `qa_sheets.py`). It removes 35 non-person "face" readings (horses, dogs,
   monkeys, a sheep, turtles, object blobs, one neck patch), measures arm/leg skin for 6
   people seen from behind, drops one limb reading that caught a red object, and removes
   duplicate readings of the same face (by overlap of the smaller box > .5).

**Caveats**: ITA bands (Chardon et al.) were built for photographed skin. Here ITA is a
continuous lightness/yellowness index for stylised art, not a claim about the ethnicity of
the characters. Shading and blush pull individual readings by a few degrees, but the
ordering matched visual inspection throughout QA.

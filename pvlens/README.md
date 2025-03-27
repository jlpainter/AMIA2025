# AMIA2025

## Title  
**PVLens: Enhancing Pharmacovigilance Through Automated Label Extraction**

## Authors  
Jeffery L. Painter, Greg Powell, and Andrew Bate

## Abstract

Reliable drug safety reference databases are essential for pharmacovigilance, yet existing resources like SIDER are outdated and static. We introduce PVLens, an automated system that extracts labeled safety information from FDA Structured Product Labels (SPLs) and maps terms to MedDRA. PVLens integrates automation with expert oversight through a web-based review tool. In validation against 97 drug labels, PVLens achieved an F1 score of 0.882, with high recall (0.983) and moderate precision (0.799). By offering a scalable, more accurate and continuously updated alternative to SIDER, PVLens enhances real-time pharamcovigilance with improved accuracy and contemporaneous insights.

## Preprint Link

https://arxiv.org/abs/2503.20639

---

This repository contains the original source data used in the validation of the **PVLens** pipeline for extracting indications, adverse events, and black box warnings from FDA Structured Product Labels (SPLs).

# 📁 Directory Structure

## Data

- **dictionary/**: Contains mapping files like `guid_to_xml.csv`, which links each GUID to its corresponding SPL XML filename.
- **extracted_labels/**: Output of the PVLens pipeline, split into three sub-directories:
  - `indications/`
  - `aes/` (adverse events)
  - `blackbox/` (black box warnings)  
  Each contains one `.txt` file per SPL, named by GUID.
- **src_labels/**: The original XML files of SPLs downloaded from the FDA and used as the input source for PVLens.
- **results/**: Contains the manual review data used for the analysis in the manuscript.


## 📌 Notes
- Last update contains 97 SPL labels that have been manually reviewed and adjudicated.
- The `extracted_labels/` directory includes the output of PVLens categorized by label type.
- All GUIDs correspond to entries in `guid_to_xml.csv` for traceability.

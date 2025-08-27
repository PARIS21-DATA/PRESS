# PRESS

This repository contains the code for creating PRESS from OECD CRS data. The methodology combines purpose codes and keyword matching on project titles with classification by a fine‑tuned large language model on project descriptions to produce the final PRESS outputs. For the full methodology note, see the documentation page.

## Repository structure

Folder structure:

- `data/` — Working data area used by the notebooks and scripts
  - `raw/` — Read-only raw CRS data
  - `processed/` — Intermediate data produced
  - `output/` — Used for final datasets and any other outputs
  - `models/` — To store models used during text processing as well as fine-tuned LLMs for classification
  - `keywords/` — Keyword dictionaries used for title matching and topic focus
  - `auxiliary/` — Auxiliary data that contains information added to final PRESS

- `notebooks/` — Jupyter notebooks that implement the workflow
  - `final/` — Production notebooks to run in order:
	 1. `A_title_pattern_matching.ipynb` — Applies purpose‑code filters and keyword matching to project titles
	 2. `B_colab_predict_with_finetuned_model.ipynb` (on Google Colab) — Uses a fine‑tuned large language model to classify project descriptions
	 3. `C_merge_predictions.ipynb` — Merges the rule‑based candidates with LLM predictions
	 4. `D_create_press.ipynb` — Creates the final PRESS dataset

	 not part of the workflow:
	 - `visual_checks_and_diagnostics.ipynb` — Visualizations and diagnostics that can be used at different stages of the pipeline
     - `colab_classify_....ipynb` — Self-contained notebooks that can be uploaded to Colab and used to train at a fine-tuned model from scratch for statistics and gender 
  - `development/` — All notebooks as they have been used to set up the pipeline. Kept for completness.

- `docs/` — Documentation sources (MkDocs site configuration and reference material). Contains PDFs/PPTs describing methodology proposals and background

- `former_code/` — Historical R scripts used in earlier versions of the workflow. Useful for reference; not part of the current production run

- `src/` — Python package modules supporting the notebooks for CRS download and text processing

Other files:

- `pyproject.toml` — Python project metadata and dependencies
- `mkdocs.yml` — Configuration for building the documentation site


## How to use this repository

### Prerequisites

- Python 3.7 or higher (as specified in `pyproject.toml`)
- Ability to run Jupyter notebooks locally and, for the classification step, access to Google Colab (recommended for GPU acceleration)
- fastText language identification model file placed at `data/models/fasttext/lid.176.bin` (see Text processing section below)
- spaCy plus these language models installed: `en_core_web_sm`, `fr_core_news_sm`, `es_core_news_sm`, `de_core_news_sm`

To install dependencies in a fresh environment (example workflow):

1) Create and activate a virtual environment
2) Install the package with its dependencies from the `pyproject.toml`

Example:

```powershell
py -m venv .venv
.\.venv\Scripts\Activate.ps1
python -m pip install -U pip
pip install -e . -c constraints.txt
```

The `-c constraints.txt` flag applies repository-specific installation constraints. Here it ensures `blis` is installed from a prebuilt wheel (`--only-binary :all:`) instead of compiling from source. This matters on Windows because `blis` (a spaCy dependency) otherwise requires a local C/C++ build toolchain; using wheels avoids those build errors and speeds up installation.

### End‑to‑end workflow

Run the notebooks in `notebooks/final/` in the order below. Each notebook documents its inputs and writes outputs back to `data/processed/` or `data/output/`.

1) **Notebook A**: Title pattern matching 
	 - Loads CRS from `data/raw/` and uses `src/text_processing.py` to normalize titles, detect language (fastText), and lemmatize (spaCy)
	 - Applies purpose‑code filters and keyword/acronym rules per language (EN/FR/ES/DE)
	 - Produces candidate tables with columns such as normalized/lemmatized titles, detected language, matched keywords/acronyms, and blacklist flags
	 - Outputs:
		 - `crs_lemmatized_titles_wo_stopwords.feather` (intermediate dataset as checkpoint)
		 - `crs_titles_matched_wo_stopwords.feather` (output of title pattern matching) 
         - `stat_to_mine.feather`
         - `gen_to_mine.fether`
         - `conflicting_descr_stat.feather`
         - `conflicting_descr_gen.feather`

2) **Notebook B Colab**: Predict with fine‑tuned model 
	- Upload `notebooks/final/colab_predict_with_finetuned_model.ipynb` to Google Colab. 
    - Upload `.._to_mine.feather` and `conflicting_descr_....feather` datasets
    - Upload model checkpoints (found in shared Teams PRESS folder)
	- Set configuration cell with all paths to data and model checkpoints
    - Runs batched inference with Hugging Face Trainer using the fine-tuned models
    - Outputs: 
		- `unlabeled_predicted_<stat/gen>.feather`
		- `conflicting_predicted_<stat/gen>.feather`

3) **Notebook C**: Merge LLM predictions back to CRS
	 - Inputs:
		 - `crs_titles_matched_wo_stopwords.feather`
		 - Predicted descriptions exported from Colab saved under `data/processed/predicted/`
	 - Merges predictions with results of keyword matching
	 - Writes entire merged data to `data/output/crs_predicted.feather` 

4) **Notebook D**: Create PRESS
	 - Sets threshold on statistics and gender and adds additional information from `data/auxiliary/`
	 - Writes final outputs to `data/output/` 

- **Visual checks and diagnostics**: `notebooks/final/visual_checks_and_diagnostics.ipynb` provides QA and diagnostics:
	- Descriptive stats and histograms on CRS 
	- Inspection of title‑matching outputs 
	- Keyword frequency plots by language and simple threshold sensitivity plots for probabilities.
    - ...

### Text processing (fastText + spaCy)

The module `src/text_processing.py` handles multilingual preprocessing for titles/descriptions: normalization, language ID, lemmatization, and rule‑based keyword/acronym detection. It loads its resources at import time, so missing models will raise errors until installed.

- What it provides
	- `normalize_str`, `remove_accents` — cleans text and removes diacritics
	- `detect_language` — uses fastText LID (`lid.176.bin`) to return ISO‑like labels (e.g., `en`, `fr`)
	- `lemmatize_str`, `lemmatize_batch` — spaCy‑based lemmatization per language; optional stopword removal
	- `detect_keywords`, `detect_acronyms` — finds exact matches from language‑specific columns of a DataFrame
	- `process_keywords` — normalizes/lemmatizes keyword lists for `en`, `fr`, `es`, `de`, with special handling for German hyphens and automatic de‑accented variants

- fastText (language identification)
	- Install: `pip install fasttext-wheel`
	- Model: Download `lid.176.bin` (fastText official LID model) and store at repository path `data/models/fasttext/lid.176.bin`
	- The module loads it via a relative path from `src/` to `data/` (ensure the file exists at the path above)
	- Behavior: `fasttext_model.predict(text, k=1)`; output label like `__label__en` is converted to `en`

- spaCy (tokenization/lemmatization)
	- Install: `pip install spacy`
	- Required models (loaded at import): `en_core_web_sm`, `fr_core_news_sm`, `es_core_news_sm`, `de_core_news_sm`
	- Install models (PowerShell):
		```powershell
		python -m spacy download en_core_web_sm
		python -m spacy download fr_core_news_sm
		python -m spacy download es_core_news_sm
		python -m spacy download de_core_news_sm
		```
	- Notes: The module customizes stopwords to keep “cuenta” (es) and “mine” (en) meaningful in domain phrases.

### Documentation

The `docs/` folder and `mkdocs.yml` define a static documentation site (MkDocs with the Material theme). The folder also contains background PDFs/PPTs on PRESS methodology modernization.

### Potential directions for future devlopment

- modularization of Colab classify notebooks into helper functions with export to `src` as e.g. `train.py`, `data.py`, ...
- generalization of workflow to generic topics instead of hard-coded statistics and gender, e.g. climate change


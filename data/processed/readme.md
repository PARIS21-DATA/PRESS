# Processed data overview

This folder contains intermediate datasets produced along the PRESS workflow. Subfolders and their purpose:

- `manually_corrected/`
	- Manually adjusted training dataset used to refine the model. Includes corrections of false positives and targeted manual additions to rebalance and focus the training set.

- `title_matched/`
	- CRS data after applying title‑based keyword and acronym matching (and related preprocessing). These are the outputs from Notebook A used to identify candidate records.

- `prediction_sets/`
	- Datasets prepared in Notebook A that contain the descriptions to classify (unlabeled or conflicting). Upload these files to Google Colab for inference in the fine‑tuned model notebook.

- `predicted/`
	- Classified description outputs downloaded from Colab after running the prediction notebook. These contain model probabilities (e.g., probability_is_statistics / probability_is_gender) used for the merging step.


# Press Technical Methodology Note 

All technical documentation concerning the PRESS methodology modernization.

## Project layout

    data/
        raw/                # contains read-only data  
        processed/          # contains intermediate datasets 
        output/             # contains the final output data
    former_code/
        Yu/                 # contains former code from previous PRESS rounds 
    src/                    # the utility functions used for the PRESS pipeline 
        download_crs.py     # contains functions to bulk-downlaod the entire CRS data set.
        text_processing.py  # contains all functions to process text.
    notebooks/
        development/        # contains the current state of development
        final/              # contains the final pipeline
    docs/
        index.md            # The documentation homepage.
        about.md            # Other markdown pages, images and other files.
    pyproject.toml          # configuration file with requirements for env
    mkdocs.yml              # The mkdocs configuration file.

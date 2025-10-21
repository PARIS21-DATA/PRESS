Building on the methodological considerations outlined in Section [2](02_data_source.md), PARIS21 has developed and applied text analysis techniques to address the challenges of using the CRS to measure funding to statistics. The earlier methodology, based on analysing the frequency of key statistical terms in project titles and descriptions, provided a solid foundation for identifying relevant projects beyond the SCB purpose code and has proven effective in tracking support to statistical development over time.  

Recognising the transformative potential of AI and, in particular LLMS, PARIS21 undertook an extensive methodological modernisation in 2025. By leveraging the contextual understanding and multilingual capabilities of this technology, the refined approach enables a more accurate and comprehensive identification of statistical components across diverse project descriptions. The use of AI now allows PARIS21 to capture the true objectives of projects with higher confidence and classify them according to their relevance to statistical capacity development, demonstrating how innovative analytical tools can strengthen evidence-based decision-making in the field of international cooperation and contribute to a more nuanced understanding of support to statistics.

At the core of the methodology lies a two-step analytical process that combines targeted filtering with AI-supported classification. In the first step, projects are pre-identified as statistical based on their assignment to the SCB purpose code and the presence of statistical keywords in project titles. This filtering process operates under the assumption that projects explicitly referencing statistical terms in their titles are likely to have a primary statistical focus. The resulting subset of verified statistical projects serves as a training dataset for the subsequent application of an LLM. In the second step, the trained model is applied to the remaining CRS dataset to detect projects with significant statistical components, even when these are not explicitly labelled or coded as such.

To ensure comparability over time, the complete methodology is applied retrospectively for all previous years.


## 3.1 Pre-identification of Statistical Projects

All CRS records were first filtered using SCB purpose code 16062, which captures projects explicitly reported as supporting statistical systems. The subsequent text analysis of project titles then follows a structured, language-sensitive process designed to maximise consistency and accuracy across different reporting languages by using techniques from the field of natural language processing (NLP).[^8] 

[^8]: For all technical details outlined in this subsection, please see the [pre-identification notebook](https://github.com/PARIS21-DATA/PRESS/blob/main/notebooks/final/A_title_pattern_matching.ipynb).

##### Text preparation

All project titles were first **normalised**, removing punctuation, upper cases, symbols, and excessive spacing while preserving essential linguistic elements such as apostrophes and accented characters. This ensures that words like “survey,” “census,” or “statistical system” are consistently recognised regardless of formatting or language-specific variations. Next, the project titles were **lemmatised**[^9], meaning that words were reduced to their root or dictionary form (for example, “surveys,” “surveyed,” and “surveying” all become “survey”). Lemmatization allows the analysis to recognise variations of the same concept and to match keywords more accurately. The process was applied using language-specific models for the most prevalent languages English, French, Spanish, and German, ensuring that equivalent terms in different languages were treated consistently. Lastly, so called stopwords were removed which are extremely common words, like "the" "a" "is" and "and", which carry low semantic value.

[^9]: Lemmatization was implemented through the NLP package [spaCy](https://spacy.io/).

##### Keyword matching

A list of statistical keywords curated by experts working in the field of official statistics (like "census", "civil registration", see Apendix [Keywords and Acronyms](appendix_keywords_acronyms.md#keywords)) was then used to detect projects whose titles explicitly contain one of the statistical keywords. To prevent false positives, a blacklist of keywords was applied to exclude projects that could misleadingly appear statistical but are unrelated to data activities (for example, “landmine” in the context of landmine surveys). In addition to the terms on the blacklist, projects coded under purpose codes 15250 (“Removal of land mines and explosive remnants of war”) and 93010 (“Refugees/asylum seekers in donor countries [non-sector allocable]”) were excluded, as funds reported under these categories are not directed towards statistical systems.

Because acronyms often represent important statistical initiatives (such as DHS or CRVS, see Appendix [Keywords and Acronyms](appendix_keywords_acronyms.md#acronyms)), they were treated separately. Acronyms were detected in non-normalized and non-lemmatized project titles, ensuring that project titles containing recognised statistical abbreviations were correctly identified.

Finally, each project identified either by the purpose code 16062 or by the presence of statistical keywords or acronyms in the project title was flagged as a statistical project. This pre-identified group forms the foundation for the next stage of the methodology.


## 3.2 AI-based Classification by Relevance to Statistics

Building on the pre-identified set of statistical projects described in Section [3.1](03_identifying_stat_projects.md#31-pre-identification-of-statistical-projects), the second stage applies the large multilingual transformer model XLM-RoBERTa[^10] to recognise projects that include a substantial statistical component, even when not explicitly labelled or coded as such.[^11]

[^10]: The XLM-RoBERTa [(Conneau et al., 2019)](https://arxiv.org/pdf/1911.02116) by Meta AI was pre-trained on a 2.5TB CommonCrawl corpus covering 100 languages. It was chosen due to its widespread use and strong performance on multilingual text in comparison to other multilingual models like mBERT (see [Hu et al., 2020](https://arxiv.org/pdf/2003.11080), [Goyal et al., 2021](https://arxiv.org/pdf/2105.00572v1)).

[^11]: For all technical details outlined in this subsection, please see the [classification notebook](https://github.com/PARIS21-DATA/PRESS/blob/main/notebooks/final/colab_classify_statistics.ipynb).

##### Building the Training data

To provide the model with sufficient contextual information, the project’s short and long descriptions were combined into a single text input in the format “short description: long description.” This ensures that the model is supplied with both concise and detailed aspects of project documentation, allowing for more accurate understanding of project objectives. This description combination was, however, not normalized or lemmatized, since modern transformer architectures such as XLM-RoBERTa process text through built-in tokenization and are designed to handle natural language directly, capturing morphological and contextual information without requiring explicit lemmatization [(Toporkov and Agerri, 2024)](https://arxiv.org/pdf/2302.00407).

The pre-identified set of clearly statistical projects served as positive training examples. However, as there are no explicitly labelled non-statistical projects in the CRS to serve as negative examples in the training set, a strategy was needed to identify reliable negative samples. Initially, a set of negative (non-statistical) examples was drawn from the remainder of the CRS database using stratified random sampling under the assumption that the vast majority of projects has no statistical component. Stratification ensured that the sampled negatives reflected the overall distribution of description lengths and reporting language, thereby avoiding bias that might arise if negatives were drawn unevenly. Then, an iterative training process was implemented:

1.	An initial model was trained using the pre-identified positives and the stratified sample of negatives.
2.	The model was then applied to the unlabelled portion of the database to identify reliable negatives—projects confidently predicted as non-statistical.
3.	These reliable negatives were incorporated into a refined training dataset to improve model precision and generalisability.

To further enhance the robustness of the model, **manual verification** was performed on a subset of the training data. This human review confirmed the accuracy of model classifications and helped identify topics that were underrepresented among the positives (e.g. birth registration) or overrepresented (e.g. information systems). The verified dataset was then **refocused**     by adding or removing suitable description combinations to ensure balanced coverage of different statistical domains, preventing the model from overfitting to specific themes.

##### Model Optimisation and Application

Model performance was optimised through hyperparameter tuning using 30 candidate configurations trained on a partial dataset. This process systematically explored the influence of different parameter combinations — such as learning rate, number of epochs, or the weight decay — on classification precision. The best-performing configuration was then applied in a final training round on the complete, verified training dataset.[^12]

[^12]: See model evaluation in Appendix [Model Evaluation](appendix_model_evaluation.md).

The resulting model was subsequently used to classify all remaining unlabelled CRS projects. As PARIS21 issues the PRESS on an annual basis, the trained model was stored and can be reapplied in future PRESS cycles, enabling the simple and consistent classification of new data releases with little manual intervention. 

The finetuned model produces, for each project description, a **probability score** indicating how closely the text resembles the linguistic and contextual structure of projects in the positive (statistical) training set. This score can be interpreted as the model’s degree of confidence that a given project includes a substantial statistical component. In practice, higher scores reflect a stronger similarity to the patterns and terminology characteristic of statistical activities — such as references to data collection, surveys, censuses, or statistical systems — while lower scores indicate that the project text aligns more closely with non-statistical projects in the training data. These probability scores thus serve as a quantitative measure of the model’s classification certainty and provide a transparent basis for determining which projects are considered as supporting statistical development.


##### Prediction results

The model’s ability to handle linguistic complexity and contextual nuance across diverse reporting situations is illustrated through some examples in Table [1](#table-1-contextual-nuance). A project description in Spanish, containing numerous encoding errors and incomplete words, was nevertheless correctly identified as statistical, demonstrating the model’s robustness to imperfect text inputs and its ability to generalise meaning even under suboptimal data quality conditions. Similarly, a project reported in Dutch — a language not explicitly represented in the training process — was accurately classified, underscoring the strength of the multilingual architecture and its capacity to transfer knowledge across languages. In another case, a project written in English with an uninformative title and a purpose code unrelated to statistics was still recognised as having a major statistical component, reflecting the model’s contextual understanding. Conversely, the model assigned a low probability score to a wildlife conservation program censusing lions, correctly distinguishing the use of the word “census” in this context from its use in the context of official statistics. Collectively, these examples highlight the model’s ability to interpret meaning, context, and linguistic variety in a way that allows to measure funding towards statistical systems with high accuracy.

<h6 id="table-1-contextual-nuance">Table 1: Examples of handling of contextual nuance in various reporting situations.</h6>
{{ read_excel('./assets/table_1.xlsx', keep_default_na=False, na_filter=False) }}

##### AI-based Classification of Projects by Gender Relevance

As PARIS21 has a long history of promoting gender equality within the context of statistical systems worldwide, a gender classification was developed following the same process as the statistics classification, except for two adjustments.

First, the pre-identification of gender-focused projects drew on a comprehensive combination of criteria previously used in PRESS analyses. These included gender-related purpose codes,[^13] gender-specific keywords and acronyms (see Appendix [Keywords and Acronyms](appendix_keywords_acronyms.md#acronyms)), the gender equality and RMNCH policy markers, donors focused on gender issues, as well as funding channels primarily dedicated to gender equality, such as UN Women and other specialised organisations. This approach ensured that the pre-identified set captured the full range of projects explicitly targeting gender-related objectives.

[^13]: Purpose codes 15170 “Women's rights organisations and movements, and government institutions” and 15180 “Ending violence against women and girls” were used.

Second, for the training dataset, the selection was refined to enhance thematic precision for the subsequent training process. The training set was limited to projects flagged only by the gender equality marker[^14] and those containing gender-related keywords and acronyms,[^15] without taking projects flagged by the other gender-specific variables into account. This narrower focus ensured that the model learned from text descriptions genuinely centred on gender-related themes, thereby minimising the inclusion of projects with broad or ambiguous objectives.

[^14]: Only projects with a gender equality marker indicating gender equality as the principal objective were selected.
[^15]: Projects flagged only by the term “women” were deliberately excluded for the training set construction due to its high frequency and use across areas not exclusively concerned with gender issues.

The subsequent AI-based classification process—including model training, iterative refinement, and optimisation—followed the same procedure as that applied to the classification of projects by their relevance to statistics.

## 3.3 Creation of the Final PRESS dataset 

After obtaining a certainty score for every project in the CRS which was not previously pre-identified — reflecting the probability that it relates to statistics — the final inclusion in the PRESS dataset was determined by applying a certainty threshold.[^16] Projects with scores above the threshold were classified as statistical (or gender-related, respectively), while those below were excluded. The selection of this threshold followed a two-step process. First, the Receiver Operating Characteristic (ROC) curve on the test set was used to identify the interval offering the best trade-off between the true positive rate and the false positive rate. Second, this interval was refined through manual inspection of project descriptions located near the boundary, ensuring that the chosen threshold reflected both precision and expert judgement. This process resulted in a threshold of 0.9994 for statistical relevance and 0.991 for gender relevance.

[^16]: For all technical details outlined in this section, please consult the [dataset creation notebook](https://github.com/PARIS21-DATA/PRESS/blob/main/notebooks/final/D_create_press.ipynb).

The certainty score itself can be interpreted along a continuum — from core statistical projects such as support to censuses or national statistical strategies, to projects with a major or moderate data component, and finally to those unrelated to statistics. The higher threshold for statistics reflects the need to more strictly exclude projects with only a minor data element, which are increasingly common in ODA activities.

In a second validation step, projects in the top funding bracket — defined as those above the 95th funding percentile — were manually reviewed to verify the model’s output and correct potential misclassifications. This targeted quality assurance step ensured that the largest financial contributions were accurately represented, further enhancing the overall reliability of the final PRESS dataset.

Finally, in terms of financial instruments, the PRESS dataset is limited to standard grants, standard loans, and shares in collective investment vehicles, as these instruments represent the core and most consistently reported forms of financial flows in the CRS,[^17] ensuring comparability across donors, years, and sectors. In line with this, private sector instruments, which include loans to the private sector, guarantees, equity investments, mezzanine finance instruments and reimbursable grants, were excluded since these instruments differ from traditional ODA flows in both financial structure and development intent, as they primarily aim to mobilise private investment rather than provide direct budgetary or technical support to public statistical systems.

[^17]: For a complete list of financial instruments in the CRS, please consult the [CRS codebook](https://webfs.oecd.org/oda/DataCollection/Resources/DAC-tables-CRS-codebook.xlsx).



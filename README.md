# Handbook_CSS_TM

This repository contains code and data to reproduce our contribution to the *Handbook of Computational Social Science* on [Text Mining and Topic Modeling](https://www.taylorfrancis.com/chapters/edit/10.4324/9781003025245-24/text-mining-topic-modeling-raphael-heiberger-sebastian-munoz-najar-galvez?context=ubx&refId=a1727d76-7a53-4475-9e19-c368d84e864d).

If you use any of the code or ideas presented here, please cite the paper as follows:

* Heiberger, Raphael H., & Sebastian Munoz-Najar Galvez. (2021). Text mining and topic modeling. In Uwe Engel,Anabel Quan-Haase, Sunny Xun Liu, Lars Lyberg (Eds.), *Handbook of Computational Social Science*. London: Routledge.  


## Short description
Working with text poses important conceptual and methodological challenges. Topic models are a popular tool to reduce texts’ complexity and find meaningful themes in large corpora. After an overview of existing work, we explain how to employ structural topic models, one of the variations of topic modeling of most relevance to social researchers. In particular, however, this chapter emphasizes the selection of an appropriate number of topics K and its relation to preprocessing. We investigate the influence of preprocessing decisions on (i) the choice of K and (ii) the quality of a topic model (i.e., its predictive power and consistency). For that purpose, we examine a multitude of model setups by employing both established metrics and innovative measures. From our empirical results, we derive several practical recommendations for researchers and provide easy-to-use code to approximate an appropriate number of topics and test the robustness of one’s choice. We develop these arguments with comprehensive data on over 137,000 education-related dissertations completed at U.S. universities.

## Code
Due to legal reasons, the repository cannot include the original text data from ProQuest. The raw text data is available under this [link](https://www.proquest.com/).

Nevertheless, we provide the code necessary to reconstruct everything we have done, and, apply it to own use cases. The code contains 2 scripts:

* `Handbook_Preprocessing.R`: applies 12 different preprocessing steps as discussed in the chapter and shows how to run the respective STMs
* `Handbook_Analyze_K.R`: derives the figures presented in the chapter
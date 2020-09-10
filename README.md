# CT_transport

Identifying and visualising inequities of access to clinical trials: a proof-of-concept using public transportation data
------------------------------------------------------------------------------------------------------------------------

Clinical trials (CTs) are essential to the NHS in Scotland.  They permit the patient population early access to potentially improved methods of prevention, diagnosis and treatment; inform strategic/logistic decision-making within the NHS with regards to healthcare delivery and [may even lead to improved standards of care](https://pubmed.ncbi.nlm.nih.gov/18362259/) in participating hospitals more generally.  The [Scottish Government’s Health and Social Care Delivery Plan](https://www.gov.scot/binaries/content/documents/govscot/publications/strategy-plan/2016/12/health-social-care-delivery-plan/documents/00511950-pdf/00511950-pdf/govscot%3Adocument/00511950.pdf) recognises the importance of research in high-performance health systems, with increased access to CTs an explicit aim.

It is widely recognised that participation in CTs is not uniform across [gender](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1761670/), [ethnicity](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1324792/) or [socioeconomic status](https://pubmed.ncbi.nlm.nih.gov/11956272/).  As such, health inequalities that may already exist between patient groups may be further exacerbated by disparities in CT access.  Public Health Scotland (PHS) was launched in April 2020 specifically in order [to tackle the significant and persistent health inequalities in Scotland](https://publichealthreform.scot/public-health-scotland/about-public-health-scotland/public-health-scotland-overview).  Therefore, any action that PHS takes in order to help meet the objective of increasing access to CTs should actively consider equity of access.

One barrier to healthcare access, and by extension CT participation, is [transportation](https://pubmed.ncbi.nlm.nih.gov/23543372/).  Transport is critical to the timely delivery of appropriate diagnosis or treatment of disease and barriers to transportation will inhibit recruitment, retention and/or attendance of CT participants. This project proposes the collection, analysis and visualisation of local transport data in order to understand equity of transportation access to Scottish hospitals.
There are three phases to this project:

1. Generate synthetic trial datasets, capturing transport and attendance statistics
2. Interactive visualisation of these data using R Shiny
3. Building statistical models of retention and/or attendance

The overarching aim is to define a workflow for incorporating geospatial information into our CT pipeline to characterise sites of interest and to present this data in an interactive way, using transport data as a proof-of-concept.  The longer term intention is that this workflow would be used as standard in our pipeline to evaluate whether geospatial data (e.g., deprivation indices) impacts on patient recruitment, retention and/or attendance.

The ultimate aim is to use these these data and the resulting modelling techniques to ask the question _which patients are at high risk of dropping out?_  If trials units can predict this with confidence then we may be able to intervene early and maintain participant retention.

Completed as part of the [Data Science Accelerator Programme](https://www.thedatalab.com/news/data-science-accelerator-programme/) 2020.

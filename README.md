<div id="top"></div>

<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/othneildrew/Best-README-Template">
    <img src="https://images.squarespace-cdn.com/content/v1/556deb8ee4b08a534b8360e7/1605302852735-UUOFORQ4RFJ3LOEKMO8K/verse22.png?format=750w" alt="Logo" width="750" height="236">
  </a>

  <h1 align="center">Read Me - VERSE Equity Tool</h3>

  <p align="center">
    Set up, use, and interpret the results of the VERSE Equity Tool
    <br />
    <a href="https://github.com/VERSE-Equity/VERSE-DHS/blob/main/Guide%20-%20How%20to%20use%20the%20VERSE%20Equity%20Tool%20to%20analyze%20DHS%20data.pdf"><strong>Open the User Guide »</strong></a>
    <br />
    <br />
    <a href="https://immunizationeconomics.org/verse-home">Learn about the project</a>
    ·
    <a href="https://jh.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=6a5667b4-6ca2-4e47-9ff6-ade4010b2a84">View Demo</a>
    ·
    <a href="mailto:bpatenaude@jhu.edu">Report Bug</a>
    ·
    <a href="mailto:bpatenaude@jhu.edu">Request Feature</a>
  </p>
</div>

<br />
<br />

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#about-the-verse-dhs-repository">About the VERSE-DHS Repository</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
        <li><a href="#model-setup">Model Setup</a></li>
        <li><a href="#available-vaccines">Available vaccines</a></li>
      </ul>
    </li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>


<br />
<br />

<!-- ABOUT THE PROJECT -->
## About The Project

<p>As low and middle-income countries expand domestic investment in vaccines, data on the economic value of these investments can help decisionmakers compare the benefits of vaccination with other health and non-health investments. These data can also provide advocates with rigorous evidence to demonstrate the impact of vaccines in terms that are comparable with other sectors. Additionally, it is crucial to understand inequities in access and coverage and how inequitable vaccine coverage may impede full the realization of the benefits of vaccines at the national level.</p>
<br>
<div align="center">
<a href="https://youtu.be/zqxSvMsl9dk">
    <img src="https://img.youtube.com/vi/zqxSvMsl9dk/0.jpg" alt="Logo" width="480" height="360">
  <p>Click to access the video</p>
  </a>
</div>
<br>
<p><a href="https://immunizationeconomics.org/verse-home"><strong>Learn more about the VERSE project »</strong></a></p>

<p align="right">(<a href="#top">back to top</a>)</p>



### About the VERSE-DHS Repository
<p>This repository contains the most up-to-date generic VERSE Equity Tool for DHS data analysis. It should only be used as a <strong>reference</strong> and should not be altered in any way.</p>
<p>Customizable versions of the VERSE Equity Tool exist in other repositories. Please contact Mr. Gatien de Broucker to access or set up a separate repository: <a href="mailto:gdebroucker@jhu.edu">gdebroucker@jhu.edu</a></p>

<p align="right">(<a href="#top">back to top</a>)</p>


<!-- GETTING STARTED -->
## Getting Started




### Prerequisites

<p>You must have R, R-Studio and Git-for-Windows installed on your computer and up-to-date.</p>

- [x] <a href="https://cran.r-project.org/bin/windows/base">Download R</a>
- [x] <a href="https://www.rstudio.com/products/rstudio/download">Download R-Studio</a> · Note: the free open-source license is enough
- [x] <a href="https://gitforwindows.org">Download Git-for-Windows</a>

<p align="right">(<a href="#top">back to top</a>)</p>


### Installation

1. In R-Studio, create a new project (<i>File >> New Project...</i>)
2. In the <i>New Project Wizard</i> in R-Studio, select <i>Version Control >> Git</i>
3. Paste the following URL to <i>Repository URL</i> and designate a folder where the VERSE program will be copied to
   ```
   https://github.com/VERSE-Equity/VERSE-DHS.git
   ```
4. Open <i>VERSE.R</i> (you can type the following code in R-Studio)
   ```r
   file.edit("VERSE.R")
   ```
5. Install the necessary R packages
   ```r
   install.packages("ggrepel")
   install.packages("prevR")
   install.packages("sf")
   install.packages("matchmaker")
   install.packages("rmapshaper")
   install.packages("PHEindicatormethods")
   install.packages("radiant")
   install.packages("StatMeasures")
   devtools::install_github("ropensci/rdhs")
   ```

<p><a href="https://happygitwithr.com/rstudio-git-github.html">Read more about using Git and GitHub here »</a></p>

<p align="right">(<a href="#top">back to top</a>)</p>


### Model Setup
<p>Once the <i>VERSE.R</i> program is opened in R-Studio, you must specify several model inputs based on your analysis query and as appropriate for the country of interest:</p>

1. <strong>What is your working directory?</strong> The working directory determines where you want the VERSE outputs to be stored on your computer. It should be different from the clone repository, which only stores the program.
   ```r
   setwd("C:/YOUR DIRECTORY HERE")
   ```

2. <strong>Which country do you want to analyze? And for what year of the DHS?</strong> For both questions above, you should review the <a href="https://dhsprogram.com/Countries">availability of DHS data</a>.

   ```r
   COUNTRY <- "Uganda"
   YEAR <- 2016
   ```

3. <strong>Which vaccines to include?</strong> A <a href="#available-vaccines">list of available vaccines</a> is displayed below. Only select vaccines that were included in the <a href="https://apps.who.int/immunization_monitoring/globalsummary/schedules">national immunization schedule</a> the year the DHS survey was performed.
   ```r
   VACCINES <- c("BCG","DTP1","DTP2","DTP3","OPV1","OPV2","OPV3","ZERO","FULL","COMPLETE")
   ```

4. <strong>Do you want to generate maps?</strong> Maps convey excellent insights on geographic disparities, but they add to the analysis time. You can deactivate them to make the program run faster.
   ```r
   MAP = "YES"
   ```


You can also search for `ACTION NEEDED` in <i>VERSE.R</i> to review where these inputs are needed.

<p align="right">(<a href="#top">back to top</a>)</p>


### Available vaccines

| Vaccine                       | Code (birth dose) | Code (dose 1) | Code (dose 2) | Code (dose 3) |
|-------------------------------|-------------------|---------------|---------------|---------------|
| Bacillus Calmette–Guérin |  | BCG |  |  |
| Diphtheria-Tetanus-Pertussis |  | DTP1 | DTP2 | DTP3 |
| Oral or Inactivated Polio | PolioBD | OPV1 | OPV2 | OPV3 |
| Inactivated Polio only |  | IPV1 | IPV2 | IPV3 |
| Pentavalent |  | Penta1 | Penta2 | Penta3 |
| Measles |  | MCV1<sup> A</sup> | MCV2<sup> A</sup> |  |
| Pneumococcal conjugate |  | PCV1 | PCV2 | PCV3 |
| Rotavirus |  | ROTA1 | ROTA2 | ROTA3 |
| Hepatitis B | HEPBBD | HEPB1 | HEPB2 | HEPB3 |
| Haemophilus influenzae type b |  | HIB1 | HIB2 | HIB3 |


<br>
<br>
<p>In addition to individual vaccines, <strong>the following health outcomes should always be specified as inputs</strong>:</p>

| Health outcome                         | Code     | Description                                                   |
|----------------------------------------|----------|---------------------------------------------------------------|
| Zero-dose status | ZERO | A child has a <i>zero-dose status</i> if they did not receive either BCG, DTP1, or OPV1 by 12 months of age |
| Fully-immunized for age status | FULL<sup> B</sup> | A child is considered <i>fully immunized for age</i> if they received all scheduled vaccines for their age (in the model, this applies to BCG, DTP1-3, OPV1-3, and any other specified vaccine) |
| Completed immunization schedule status | COMPLETE | TBD |


<p><strong>Notes:</strong></p>

<sup>A.</sup> Only include MCV1 and MCV2 if no supplemental immunization activity (or vaccine campaign) for measles took place in the two years preceding the analyzed DHS survey.

<sup>B.</sup> These vaccines and health outcomes <strong>must be included</strong> in the `VACCINES()` input.


<p align="right">(<a href="#top">back to top</a>)</p>


<!-- LICENSE -->
## License

<p>To be updated.</p>

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

Dr. Bryan Patenaude (he/him) - <a href="mailto:bpatenaude@jhu.edu">bpatenaude@jhu.edu</a>

Project Link: [https://immunizationeconomics.org/verse-home](https://immunizationeconomics.org/verse-home)

<p align="right">(<a href="#top">back to top</a>)</p>


<div id="top"></div>

<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://immunizationeconomics.org/verse-home">
    <img src="https://images.squarespace-cdn.com/content/v1/556deb8ee4b08a534b8360e7/1605302852735-UUOFORQ4RFJ3LOEKMO8K/verse22.png?format=750w" alt="Logo" width="750" height="236">
  </a>

  <h1 align="center">Read Me - VERSE Equity Tool</h3>

  <p align="center">
    Set up, use, and interpret the results of the VERSE Equity Tool
    <br />
    <a href="https://github.com/VERSE-Equity/Toolkit-DHS/blob/main/Guide%20-%20How%20to%20use%20the%20VERSE%20Equity%20Tool%20to%20analyze%20DHS%20data.pdf"><strong>Open the User Guide »</strong></a>
    <br />
    <br />
    <a href="https://immunizationeconomics.org/verse-home">Learn about the project</a>
    ·
    <a href="https://jh.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=6a5667b4-6ca2-4e47-9ff6-ade4010b2a84">View Demo</a>
    ·
    <a href="https://github.com/VERSE-Equity/Toolkit-DHS/issues">Report Bug</a>
    ·
    <a href="https://github.com/VERSE-Equity/Toolkit-DHS/issues">Request Feature</a>
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
    <li><a href="#troubleshoot">Troubleshoot</a></li>
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



### About the Toolkit-DHS Repository
<p>This repository contains the most up-to-date generic VERSE Equity Tool for DHS data analysis. It can be used, edited, and shared with proper credit, following the <a href="#license">MIT license</a>.</p>
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


## Use

### VERSE metrics

<p>The VERSE Equity Toolkit generates the following metrics:</p>
<ul>
<li><strong>Coverage ("efficiency") metric</strong>: <i>Coverage</i></li>
<li><strong>Equity metrics:</strong>
<ul style="list-style-type:circle">
  <li>Wagstaff concentration index (composite ranking): <i>CI</i></li>
  <li>Wagstaff concentration index (wealth ranking)<sup>A</sup>: <i>CI_Wealth</i></li>
  <li>Erreyger concentration index (composite ranking): <i>CI_E</i></li>
  <li>Erreyger concentration index (wealth ranking)<sup>A</sup>: <i>CI_E_Wealth</i></li>
  <li>Absolute Equity Gap: <i>AEG</i></li>
  <li>Relative Equity Gap: <i>REG</i></li>
  <li>Slope Index of Inequity: <i>SII</i></li>
  <li>Relative Index of Inequity: <i>RII</i></li>
</ul>
</li>
</ul>

<p><strong>Notes:</strong></p>

<p><sup>A.</sup> The common concentration index based on socioeconomic status ("wealth") only is also generated.</p>

<p align="right">(<a href="#top">back to top</a>)</p>


### Calling the results

After successfully running the toolkit, the coverage and equity metrics can be called using the following code:
   ```r
   results$[ENTER DESIRED METRIC AND SPECIFICATION HERE]
   ```

The code for the metrics are presented in the <a href="#verse-metrics">list above</a> (in <i>italics</i>). The codes for the specifications (national or subnational level, vaccine, and factor of inequity) are detailed in the <a href="https://github.com/VERSE-Equity/VERSE-DHS/blob/main/Guide%20-%20How%20to%20use%20the%20VERSE%20Equity%20Tool%20to%20analyze%20DHS%20data.pdf"><strong>VERSE Equity Toolkit User Guide</strong></a>.

Examples:
   ```r
   # National estimates for the Wagstaff concentration index (composite ranking) by vaccine
   results$CI_Results
   
   # Subnational estimates for the Wagstaff concentration index (composite ranking) for a specific vaccine by region
   results$CI_GEO_MCV1
   ```

<p>Consult the <a href="https://github.com/VERSE-Equity/VERSE-DHS/blob/main/Guide%20-%20How%20to%20use%20the%20VERSE%20Equity%20Tool%20to%20analyze%20DHS%20data.pdf">VERSE Equity Toolkit User Guide</a> for more details.</p>


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

<p><sup>A.</sup> Only include MCV1 and MCV2 if no supplemental immunization activity (or vaccine campaign) for measles took place in the two years preceding the analyzed DHS survey.

<sup>B.</sup> These vaccines and health outcomes <strong>must be included</strong> in the `VACCINES()` input.</p>


<p align="right">(<a href="#top">back to top</a>)</p>


## Troubleshoot

### Error in download.file

We are aware of a common issue where the program stops running and you receive the following error message in the R Console:
   ```r
   Error in download.file(url, tf2, quiet = quiet_download) : cannot open URL 'https://gis.dhsprogram.com/arcgis/rest/directories/arcgisjobs/tools/downloadsubnationaldata_gpserver/j3d3334c368334492902fa8a86a81666e/scratch/sdr_subnational_boundaries_2022-02-11.zip'
   ```

To resolve it, run the following code <strong>directly in your R Console</strong> and re-execute the VERSE Equity Toolkit:
   ```r
   get_available_datasets(clear_cache=TRUE)
   ```

<p>This procedure will clear your cache and allow you to download another DHS dataset.</p>

<p align="right">(<a href="#top">back to top</a>)</p>


### Other errors when running the program

<p>Most errors can be resolved by restarting R-Studio and ensuring that all necessary packages are installed. The VERSE Equity Toolkit requires the latest version of R and R-Studio to be installed, as well as several R packages <a href="#installation">listed above</a>.</p>

<p>If this does not resolve the error, you can report it on the <a href="https://github.com/VERSE-Equity/Toolkit-DHS/issues">dedicated GitHub page</a>. While the toolkit is designed to accommodate many deviations in the DHS data coding, it may be missing recent updates to the data.</p>

<p>Finally, the VERSE Equity Toolkit does not currently run (or requires additional inputs) for the following countries:</p>

| Country (year)  | Reason                            | Resolution                    |
|-----------------|-----------------------------------|-------------------------------|
| Any country prior to 2010 | VERSE Equity Toolkit not yet tested on pre-2010 data | We may update the tool to include these data (let us know if you are interested in using specific pre-2010 surveys) |
| Equatorial Guinea (2011) | DHS dataset is not available to the public | None |
| Sri Lanka (2016) | DHS dataset is not available to the public | None |
| Yemen (2013) | There are no map-shape files for Yemen, and there is no data on maternal education (the level will appear as 0% in the pie charts) | Set MAP = "NO" |
| Colombia (2015) | Colombia did not collect any immunization data in the 2015 DHS | None |
| Peru (2014) | DHS dataset is not available to the public | 2012 DHS dataset is available for Peru |

<p align="right">(<a href="#top">back to top</a>)</p>


### Any other issues with the outputs or program

<p>Report issues with the outputs or the program on the <a href="https://github.com/VERSE-Equity/Toolkit-DHS/issues">dedicated GitHub page</a>. Others may have encountered the same issue and any resolution will be posted there.</p>



<p align="right">(<a href="#top">back to top</a>)</p>


<!-- LICENSE -->
## License

<p>The VERSE Equity Toolkit operates under the <a href="https://github.com/VERSE-Equity/Toolkit-DHS/blob/main/LICENSE">MIT License</a>.</p>

<p>The license allows for many types of use of the VERSE Equity Toolkit. To provide credit, we recommend using the following reference:</p>

<p>Patenaude, B., Odihi, D., Sriudomporn, S., Mak, J., Watts, E., & de Broucker, G. (2022). A standardized approach for measuring multivariate equity in vaccination coverage, cost-of-illness, and health outcomes: Evidence from the Vaccine Economics Research for Sustainability & Equity (VERSE) project. <i>Social Science & Medicine, 302</i>, 114979. doi:<a href="https://doi.org/10.1016/j.socscimed.2022.114979">https://doi.org/10.1016/j.socscimed.2022.114979</a></p>

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

<ul>
<li><strong>Dr. Bryan Patenaude</strong> (he/him) at <a href="mailto:bpatenaude@jhu.edu">bpatenaude@jhu.edu</a> to discuss applications of the VERSE Equity Toolkit on primary healthcare outcomes or other health outcomes, or on alternative data sources.</li>
<li><strong>Mr. Gatien de Broucker</strong> at <a href="mailto:gdebroucker@jhu.edu">gdebroucker@jhu.edu</a> to learn about learning opportunities and VERSE-related courses</li>
</ul>

<p>Learn more about the VERSE project: <a href="https://immunizationeconomics.org/verse-home">ImmunizationEconomics.Org/verse-home</a></p>

<p>Read the news release about the VERSE Equity Toolkit: <a href="https://immunizationeconomics.org/recent-activity/2022/4/27/measuring-equity-in-vaccination-coverage-beyond-socioeconomic-status-launch-of-the-verse-equity-toolkit">Measuring equity in vaccination coverage beyond socioeconomic status: Launch of the VERSE Equity Toolkit</a> (27 April 2022)

<p align="right">(<a href="#top">back to top</a>)</p>


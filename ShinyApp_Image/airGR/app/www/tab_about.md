




<table  width="100%">
<tbody>
  <tr>
  <td width="125"><img src="fig/logo_airGRteaching_square.svg" width="90%" height="90%"></td>
  <td><h4>
<font color="#009EE0">
To obtain help regarding the use of the <strong><font color="#009EE0">airGRteaching</font></strong> or <strong><font color="#009EE0">airGR</font></strong> packages, or to suggest modifications, send an email to <font color="#003A80"><strong>airGR@irstea.fr</strong></font>
</font>
</h4>
  </tr>
</tbody>
</table>

<br>

<strong><font color="#009EE0">airGRteaching</font></strong> is a package developed in the <img src="fig/logo_R.svg" height="20"> language devoted to the use of the <strong><font color="#009EE0"><a href = 'https://webgr.irstea.fr/en/modeles/' onclick='window.open(this.href); return false;'>GR</a></font></strong> rainfall-runoff models and the <strong><font color="#009EE0"><a href = 'https://webgr.irstea.fr/en/modeles/modele-de-neige/' onclick='window.open(this.href); return false;'>CemaNeige</a></font></strong> snowmelt and accumulation model by students and teachers.
<br>
<br><strong><font color="#009EE0">airGRteaching</font></strong> is an add-on package of the <strong><font color="#009EE0"><a href = 'https://webgr.irstea.fr/en/airgr/' onclick='window.open(this.href); return false;'>airGR</a></font></strong> hydrological package.
<br>
<br>It simplifies the use of the airGR functionalities as it only requires a basic level of programming.
<br>
The <strong><font color="#009EE0"><a href = 'https://webgr.irstea.fr/en/modeles/' onclick='window.open(this.href); return false;'>GR</a></font></strong> hydrological models in a few words:
* lumped conceptual rainfall-runoff models (<strong><font color="#009EE0"><a href = 'https://webgr.irstea.fr/en/modeles/annuel-gr1a/' onclick='window.open(this.href); return false;'>GR1A</a></font></strong>, <strong><font color="#009EE0"><a href = 'https://webgr.irstea.fr/en/modeles/mensuel-gr2m/' onclick='window.open(this.href); return false;'>GR2M</a></font></strong>, <strong><font color="#009EE0"><a href = 'https://webgr.irstea.fr/en/modeles/journalier-gr4j-2/' onclick='window.open(this.href); return false;'>GR4J</a></font></strong>, **GR5J**, **GR6J** and **GR4H**)
* designed with the objective to be as efficient as possible for flow simulation at various time steps (from annual to hourly)
* their structures were developed to have warranted complexity and limited data requirements
* can be applied on a wide range of conditions, including snowy catchments (thanks to the <strong><font color="#009EE0"><a href = 'https://webgr.irstea.fr/en/modeles/modele-de-neige/' onclick='window.open(this.href); return false;'>CemaNeige</a></font></strong> snow model)
<br><br>


#### <strong><font color="#009EE0">airGRteaching</font></strong> functionalities

* Only three simple functions for a full modelling exercise:
    + data preparation
    + model calibration
    + model simulation
* Pre-defined graphical plots:
    + static plotting functions
    + mouse events and interactive graphics (using the *dygraphs* JavaScript charting library)
* Graphical interface based on a *Shiny* interface:
    + interactive flow simulation and plotting with parameters modifications
    + automatic calibration button
    + internal variables evolution graphs
    + time period selection
    + only daily models are currently available (<strong><font color="#009EE0"><a href = 'https://webgr.irstea.fr/en/modeles/journalier-gr4j-2/' onclick='window.open(this.href); return false;'>GR4J</a></font></strong>, **GR5J**, **GR6J** + <strong><font color="#009EE0"><a href = 'https://webgr.irstea.fr/en/modeles/modele-de-neige/' onclick='window.open(this.href); return false;'>CemaNeige</a></font></strong>)
    
<font color="#009EE0">See the "Functionalities" tab for examples including <img src="fig/logo_R.svg" height="17"> commands.</font>
<br><br>


#### <strong><font color="#009EE0">airGR</font></strong> functionalities

- Easy implementation on numerous catchments 
- Data requirements limited to lumped precip., temp. and streamflow time series
- One automatic calibration procedure
- A set of efficiency criteria
- Limited computation times (use of Fortran routines to run the models)
- Pre-defined graphical plots
- Outputs include simulated flow time series and internal variables
- User can implement its own models, efficiency criteria or optimization algorithms

<br><center><img src="fig/airGR_graphe_fonctions_EN.svg" width="700"></center>

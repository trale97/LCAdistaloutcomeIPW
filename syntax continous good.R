################################################################################
#### Title: Generate syntax files for continuous Z and good separation condition
#### Author: Tra T. Le
#### Last modified: May 31, 2022

rm(list = ls())
setwd("~/continuous good")
LG <- "~/LatentGOLD6.0/lg60.exe"
GenData <- function(syntaxName, infile, outfile, B, G){
  
  newSyntaxToBe <- utils::capture.output(cat(paste0("
//LG6.0//
version = 6.0
infile '", infile,"'

model
options
   maxthreads=all;
   algorithm 
      tolerance=1e-008 emtolerance=0.01 emiterations=0 nriterations=0 ;
   startvalues
      seed=0 sets=0 tolerance=1e-005 iterations=0;
   montecarlo
      seed=0 sets=0 replicates=500 tolerance=1e-008;
   output      
      parameters=first profile estimatedvalues marginaleffects;
   outfile '", outfile,"' simulation;
variables
   caseweight n1000;
   dependent Z continuous, Y1 2, Y2 2, Y3 2, Y4 2, Y5 2, Y6 2;
   independent C1, C2;
   latent
      Cluster nominal 3;
equations
   Z <- 1 + C1 + C2 + Cluster;
   Cluster <- 1 + C1 + C2;
   Y1 <- 1 | Cluster;
   Y2 <- 1 | Cluster;
   Y3 <- 1 | Cluster;
   Y4 <- 1 | Cluster;
   Y5 <- 1 | Cluster;
   Y6 <- 1 | Cluster;
   {0 1 1 1 ", B,"
    .3267 .3267 1 ", G," 1 1 
    2.197      2.197      -2.197
    2.197      2.197      -2.197
    2.197      2.197      -2.197
    2.197     -2.197      -2.197
    2.197     -2.197      -2.197
    2.197     -2.197      -2.197
    10}
end model
")))
  utils::write.table(newSyntaxToBe, paste0(syntaxName, ".lgs"), row.names = FALSE, quote = FALSE, col.names = FALSE)
}

#step 1 and one step 
Step1_OneStepSyntax <- function(syntaxName, Step1Infile, outfile1, outfile2, G){
  
  newSyntaxToBe <- utils::capture.output(cat(paste0("
//LG6.0//
version = 6.0
infile '", Step1Infile,"'

model
title '1-step';
options
   maxthreads=all;
   algorithm 
      tolerance=1e-008 emtolerance=0.01 emiterations=250 nriterations=50 ;
   startvalues
      seed=0 sets=0 tolerance=1e-005 iterations=50;
   output      
      parameters=first standarderrors profile estimatedvalues;
   outfile '", outfile1,"' classification=posterior classification=model keep Z;
variables
   dependent Y1, Y2, Y3, Y4, Y5, Y6;
   independent C1, C2;
   latent
      Cluster nominal 3;
equations
   Cluster <- 1 + C1 + C2;
   Y1 <- 1 | Cluster;
   Y2 <- 1 | Cluster;
   Y3 <- 1 | Cluster;
   Y4 <- 1 | Cluster;
   Y5 <- 1 | Cluster;
   Y6 <- 1 | Cluster;
   {.3267 .3267 1 ", G," 1 1 
    2.197      2.197      -2.197
    2.197      2.197      -2.197
    2.197      2.197      -2.197
    2.197     -2.197      -2.197
    2.197     -2.197      -2.197
    2.197     -2.197      -2.197
    }
end model

model
title 'step-1';
options
   maxthreads=all;
   algorithm 
      tolerance=1e-008 emtolerance=0.01 emiterations=250 nriterations=50 ;
   startvalues
      seed=0 sets=0 tolerance=1e-005 iterations=50;
   output      
      parameters=first standarderrors profile estimatedvalues;
   outfile '", outfile2,"' classification=posterior keep Z C1 C2;
variables
   dependent Y1, Y2, Y3, Y4, Y5, Y6;
   latent
      Cluster nominal 3;
equations
   Cluster <- 1;
   Y1 <- 1 | Cluster;
   Y2 <- 1 | Cluster;
   Y3 <- 1 | Cluster;
   Y4 <- 1 | Cluster;
   Y5 <- 1 | Cluster;
   Y6 <- 1 | Cluster;
   {0 0 
    2.197      2.197      -2.197
    2.197      2.197      -2.197
    2.197      2.197      -2.197
    2.197     -2.197      -2.197
    2.197     -2.197      -2.197
    2.197     -2.197      -2.197
    }
end model
")))
  utils::write.table(newSyntaxToBe, paste0(syntaxName, ".lgs"), row.names = FALSE, quote = FALSE, col.names = FALSE)
}

#adjusted and unadjusted step-3 using proportional and ML method
Step3Syntax <- function(syntaxName, Step3Infile, outfile1, outfile2){
  
  newSyntaxToBe <- utils::capture.output(cat(paste0("
//LG6.0//
version = 6.0
infile '", Step3Infile,"'

model
title '3-step adjusted';
options
   maxthreads=all;
   algorithm 
      tolerance=1e-08 emtolerance=0.01 emiterations=250 nriterations=50 ;
   startvalues
      seed=0 sets=0 tolerance=1e-05 iterations=50;
   step3 proportional ml simultaneous;
   output      
      parameters=first standarderrors=robust profile estimatedvalues;
   outfile '", outfile1,"' classification=model keep Z;
variables
   independent C1, C2;
   latent Cluster nominal posterior = ( Cluster#1 Cluster#2 Cluster#3 ) ;
equations
   Cluster <- 1 + C1 + C2;
end model

model
title '3-step unadjusted';
options
   maxthreads=all;
   algorithm 
      tolerance=1e-08 emtolerance=0.01 emiterations=250 nriterations=50 ;
   startvalues
      seed=0 sets=0 tolerance=1e-05 iterations=50;
   step3 modal none simultaneous;
   output      
      parameters=first standarderrors=robust estimatedvalues=model;
   outfile '", outfile2,"' classification=model keep Z;
variables
   independent C1, C2;
   latent Cluster nominal posterior = ( Cluster#1 Cluster#2 Cluster#3 ) ;
equations
   Cluster <- 1 + C1 + C2;
end model")))
  utils::write.table(newSyntaxToBe, paste0(syntaxName, ".lgs"), row.names = FALSE, quote = FALSE, col.names = FALSE)
}


#Schuler model
SchulerSyntax <- function(syntaxName, SchulerInfile, outfile){
  
  newSyntaxToBe <- utils::capture.output(cat(paste0("
//LG6.0//
version = 6.0
infile '", SchulerInfile,"'

model
title schuler;
options
   maxthreads=all;
   algorithm 
      tolerance=1e-08 emtolerance=0.01 emiterations=250 nriterations=50 ;
   startvalues
      seed=0 sets=0 tolerance=1e-05 iterations=50;
   step3 modal none simultaneous;
   output      
      parameters=first standarderrors=robust profile estimatedvalues marginaleffects append='", outfile,"';
variables
   dependent Z continuous;
   latent Cluster nominal posterior = ( Cluster#1 Cluster#2 Cluster#3 ) 
                          propensity = ( MCluster#1 MCluster#2 MCluster#3 );
equations
   Cluster <- 1;
   Z <- 1 + Cluster;
end model")))
  utils::write.table(newSyntaxToBe, paste0(syntaxName, ".lgs"), row.names = FALSE, quote = FALSE, col.names = FALSE)
}

#Yamaguchi model
YamaSyntax <- function(syntaxName, YamaInfile, outfile){
  
  newSyntaxToBe <- utils::capture.output(cat(paste0("
//LG6.0//
version = 6.0
infile '", YamaInfile,"'

model
title 'yamaguchi';
options
   maxthreads=all;
   algorithm 
      tolerance=1e-08 emtolerance=0.01 emiterations=250 nriterations=50 ;
   startvalues
      seed=0 sets=0 tolerance=1e-05 iterations=50;
   step3 proportional none simultaneous;
   output      
      parameters=first standarderrors=robust profile estimatedvalues marginaleffects append='", outfile,"';
variables
   dependent Z continuous;
   latent Cluster nominal posterior = ( Cluster#1 Cluster#2 Cluster#3 ) 
                          propensity = ( MCluster#1 MCluster#2 MCluster#3 );
equations
   Cluster <- 1;
   Z <- 1 + Cluster;
end model
")))
  utils::write.table(newSyntaxToBe, paste0(syntaxName, ".lgs"), row.names = FALSE, quote = FALSE, col.names = FALSE)
}

#Bray and Tra and naive
BrayTraNaiveSyntax <- function(syntaxName, BrayTraNaiveInfile, outfile){
  
  newSyntaxToBe <- utils::capture.output(cat(paste0("
//LG6.0//
version = 6.0
infile '", BrayTraNaiveInfile,"'

model
title 'bray';
options
   maxthreads=all;
   algorithm 
      tolerance=1e-08 emtolerance=0.01 emiterations=250 nriterations=50 ;
   startvalues
      seed=0 sets=0 tolerance=1e-05 iterations=50;
   step3 modal bch bray simultaneous;
   output      
      parameters=first standarderrors=robust profile estimatedvalues marginaleffects append='", outfile,"';
variables
   dependent Z continuous;
   latent Cluster nominal posterior = ( Cluster#1 Cluster#2 Cluster#3 ) 
                          propensity = ( MCluster#1 MCluster#2 MCluster#3 );
equations
   Cluster <- 1;
   Z <- 1 + Cluster;
end model

model
title 'bray proportional';
options
   maxthreads=all;
   algorithm 
      tolerance=1e-08 emtolerance=0.01 emiterations=250 nriterations=50 ;
   startvalues
      seed=0 sets=0 tolerance=1e-05 iterations=50;
   step3 proportional bch bray simultaneous;
   output      
      parameters=first standarderrors=robust profile estimatedvalues marginaleffects append='", outfile,"';
variables
   dependent Z continuous;
   latent Cluster nominal posterior = ( Cluster#1 Cluster#2 Cluster#3 ) 
                          propensity = ( MCluster#1 MCluster#2 MCluster#3 );
equations
   Cluster <- 1;
   Z <- 1 + Cluster;
end model

model
title 'tra';
options
   maxthreads=all;
   algorithm 
      tolerance=1e-08 emtolerance=0.01 emiterations=250 nriterations=50 ;
   startvalues
      seed=0 sets=0 tolerance=1e-05 iterations=50;
   step3 proportional bch simultaneous;
   output      
      parameters=first standarderrors=robust profile estimatedvalues marginaleffects append='", outfile,"';
variables
   dependent Z continuous;
   latent Cluster nominal posterior = ( Cluster#1 Cluster#2 Cluster#3 ) 
                          propensity = ( MCluster#1 MCluster#2 MCluster#3 );
equations
   Cluster <- 1;
   Z <- 1 + Cluster;
end model

model
title 'tra modal';
options
   maxthreads=all;
   algorithm 
      tolerance=1e-08 emtolerance=0.01 emiterations=250 nriterations=50 ;
   startvalues
      seed=0 sets=0 tolerance=1e-05 iterations=50;
   step3 modal bch simultaneous;
   output      
      parameters=first standarderrors=robust profile estimatedvalues marginaleffects append='", outfile,"';
variables
   dependent Z continuous;
   latent Cluster nominal posterior = ( Cluster#1 Cluster#2 Cluster#3 ) 
                          propensity = ( MCluster#1 MCluster#2 MCluster#3 );
equations
   Cluster <- 1;
   Z <- 1 + Cluster;
end model

model
title 'naive';
options
   maxthreads=all;
   algorithm 
      tolerance=1e-08 emtolerance=0.01 emiterations=250 nriterations=50 ;
   startvalues
      seed=0 sets=0 tolerance=1e-05 iterations=50;
   step3 proportional bch simultaneous;
   output      
      parameters=first standarderrors=robust profile estimatedvalues marginaleffects append='", outfile,"';
variables
   dependent Z continuous;
   latent Cluster nominal posterior = ( Cluster#1 Cluster#2 Cluster#3 );
equations
   Cluster <- 1;
   Z <- 1 + Cluster;
end model")))
  utils::write.table(newSyntaxToBe, paste0(syntaxName, ".lgs"), row.names = FALSE, quote = FALSE, col.names = FALSE)
}

B <- c(1,2,3)
G <- c(1,2,3)
N <- c(500, 1000, 2500)
for(b in B) {
  for(g in G) {
    for(n in N) {
      GenData(syntaxName = paste0("GenData_bi_B",b, "G",g, "N",n), infile = paste0("example",n, ".sav"), outfile = paste0("databiB",b, "G",g, "N",n, ".sav"), B = b, G = g)
      Step1_OneStepSyntax(syntaxName = paste0("Step1OneStep_bi_B",b, "G",g, "N",n), Step1Infile = paste0("databiB",b, "G",g, "N",n, ".sav"), outfile1 = paste0("data1biB",b, "G",g, "N",n, ".sav"), outfile2 = paste0("data0biB",b, "G",g, "N",n, ".sav"), G = g)
      Step3Syntax(syntaxName = paste0("Step3_bi_B",b, "G",g, "N",n), Step3Infile = paste0("data0biB",b, "G",g, "N",n, ".sav"), outfile1 = paste0("data2biB",b, "G",g, "N",n, ".sav"), outfile2 = paste0("data3biB",b, "G",g, "N",n, ".sav"))
      SchulerSyntax(syntaxName = paste0("Schuler_bi_B",b, "G",g, "N",n), SchulerInfile = paste0("data3biB",b, "G",g, "N",n, ".sav"), outfile = paste0("resultsbiB",b, "G",g, "N",n, ".csv"))
      YamaSyntax(syntaxName = paste0("Yama_bi_B",b, "G",g, "N",n), YamaInfile = paste0("data1biB",b, "G",g, "N",n, ".sav"), outfile = paste0("resultsbiB",b, "G",g, "N",n, ".csv"))
      BrayTraNaiveSyntax(syntaxName = paste0("Bray_Tra_Naive_bi_B",b, "G",g, "N",n), BrayTraNaiveInfile = paste0("data2biB",b, "G",g, "N",n, ".sav"), outfile = paste0("resultsbiB",b, "G",g, "N",n, ".csv"))
    }
  }
}



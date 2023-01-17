// This script is used to extract tabular and image information about the jets to csvs
// Run this script with (install ROOT from https://root.cern.ch first):
//    root -l -b -q RootParser.cpp
// This assumes the file CMSOpenDataJets_MC_8TeV_500K.root is in the same directory.
// The data file in root format can be downloaded from https://hot.hip.fi/index.php/2018/11/14/hip-cms-opendata-jet-tuples-8-tev-mc/

#define RootParser_cxx
#include "RootParser.h"
#include <iostream>
#include <fstream>
#include <cmath>

#define IMAGE_SIZE 18

void RootParser::Loop()
{
   fChain->SetBranchStatus("*", 0); // disable all branches
   fChain->SetBranchStatus("jetPt", 1);
   fChain->SetBranchStatus("jetEta", 1);
   fChain->SetBranchStatus("jetTightID", 1);
   fChain->SetBranchStatus("isPhysUDS", 1);
   fChain->SetBranchStatus("isPhysG", 1);
   fChain->SetBranchStatus("isPhysOther", 1);
   fChain->SetBranchStatus("QG_ptD", 1);
   fChain->SetBranchStatus("QG_axis2", 1);
   fChain->SetBranchStatus("QG_mult", 1);
   fChain->SetBranchStatus("jetGenMatch", 1);
   fChain->SetBranchStatus("nPF", 1);
   fChain->SetBranchStatus("PF_pT", 1);
   fChain->SetBranchStatus("PF_dR", 1);
   fChain->SetBranchStatus("PF_dTheta", 1);
   fChain->SetBranchStatus("jetChargedMult", 1);
   fChain->SetBranchStatus("jetNeutralMult", 1);
   fChain->SetBranchStatus("jetGirth", 1);

   if (fChain == 0)
      return;

   std::ofstream csvfile;
   csvfile.open("jets.csv");
   csvfile << "isPhysUDS, jetPt, jetGirth, jetChargedMult, jetNeutralMult, QG_ptD, QG_axis2, QG_mult" << std::endl;
   // csvfile << "isPhysUDS, jetPt, QG_ptD, QG_axis2, QG_mult" << std::endl;

   std::ofstream imgfile;
   imgfile.open("jets_img.csv");
   imgfile << "isPhysUDS";
   for (size_t i = 0; i < IMAGE_SIZE; i++)
      for (size_t j = 0; j < IMAGE_SIZE; j++)
         imgfile << ", "
                 << "pixel_" << i << "_" << j;
   imgfile << std::endl;

   Long64_t nentries = fChain->GetEntriesFast();
   Long64_t nbytes = 0, nb = 0;
   for (Long64_t jentry = 0; jentry < nentries; jentry++)
   {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0)
         break;
      nb = fChain->GetEntry(jentry);
      nbytes += nb;

      // Some basic filtering of the jets:
      if (jetEta > 2 || jetEta < -2)
         continue;
      if (jetTightID != 1)
         continue;
      if (jetGenMatch != 1)
         continue;
      if (isPhysOther)
         continue;
      if (isnan(QG_ptD))
         continue;

      csvfile << isPhysUDS << "," << jetPt << "," << jetGirth << "," << jetChargedMult << "," << jetNeutralMult << "," << QG_ptD << "," << QG_axis2 << "," << QG_mult << std::endl;
      imgfile << isPhysUDS;
      Float_t image[IMAGE_SIZE][IMAGE_SIZE] = {};
      for (size_t i = 0; i < nPF; i++)
      {
         Float_t r = PF_dR[i];
         r = -r * r * 0.666666 + r * 1.666666;
         r = r / 0.9;
         if (r >= 1)
            continue;
         Float_t theta = PF_dTheta[i];
         int x = (int)((std::sin(theta) * r + 1) * 0.5 * IMAGE_SIZE);
         int y = (int)((std::cos(theta) * r + 1) * 0.5 * IMAGE_SIZE);
         image[x][y] += PF_pT[i];
      }
      for (size_t i = 0; i < IMAGE_SIZE; i++)
         for (size_t j = 0; j < IMAGE_SIZE; j++)
            imgfile << "," << image[i][j];
      imgfile << std::endl;
   }
   csvfile.close();
   imgfile.close();
}

// This script is used to extract tabular information about the jets to a csv
// Run this script with:
//    root -l -b -q RootParser.cpp
// From the folder with the root data file downloaded from https://hot.hip.fi/index.php/2018/11/14/hip-cms-opendata-jet-tuples-8-tev-mc/
// Note that this requires that you have root installed (http://root.cern.ch/)

#define RootParser_cxx
#include "RootParser.h"
#include <iostream>
#include <fstream>
#include <cmath>


void RootParser::Loop()
{
//   In a ROOT session, you can do:
//      root> .L RootParser.C
//      root> RootParser t
//      root> t.GetEntry(12); // Fill t data members with entry number 12
//      root> t.Show();       // Show values of entry 12
//      root> t.Show(16);     // Read and show values of entry 16
//      root> t.Loop();       // Loop on all entries
//
//  This is the loop skeleton where:
//    jentry is the global entry number in the chain
//    ientry is the entry number in the current Tree
//  Note that the argument to GetEntry must be:
//    jentry for TChain::GetEntry
//    ientry for TTree::GetEntry and TBranch::GetEntry
//
    fChain->SetBranchStatus("*",0);  // disable all branches
    fChain->SetBranchStatus("jetPt",1);  // activate branchname
    fChain->SetBranchStatus("jetEta",1);  // activate branchname
    fChain->SetBranchStatus("jetGirth",1);  // activate branchname
    fChain->SetBranchStatus("jetTightID",1);  // activate branchname
    fChain->SetBranchStatus("isPhysUDS",1);  // activate branchname
    fChain->SetBranchStatus("isPhysG",1);  // activate branchname
    fChain->SetBranchStatus("isPhysOther",1);  // activate branchname
    fChain->SetBranchStatus("QG_ptD",1);  // activate branchname
    fChain->SetBranchStatus("QG_axis2",1);  // activate branchname
    fChain->SetBranchStatus("QG_mult",1);  // activate branchname
    fChain->SetBranchStatus("jetGenMatch",1);  // activate branchname
   if (fChain == 0) return;

   std::ofstream csvfile;
   csvfile.open ("jets.csv");
   csvfile << "isPhysUDS, jetPt, jetGirth, QG_ptD, QG_axis2, QG_mult" << std::endl;
   

   Long64_t nentries = fChain->GetEntriesFast();
   Long64_t nbytes = 0, nb = 0;
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0) break;
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      if (jetEta > 2 || jetEta < -2) continue;
      if (jetTightID != 1) continue;
      if (jetGenMatch != 1) continue;
      if (isPhysOther) continue;
      if (isnan(QG_ptD)) continue;
      csvfile << isPhysUDS << "," << jetPt << "," << jetGirth << "," << QG_ptD << "," << QG_axis2 << "," << QG_mult << std::endl;
   }
   csvfile.close();
}

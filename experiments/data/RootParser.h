//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Tue Nov 20 10:43:37 2018 by ROOT version 6.14/04
// from TTree OpenDataTree/OpenDataTree
// found on file: CMSOpenDataJets_MC_8TeV_500K.root
//////////////////////////////////////////////////////////

#ifndef RootParser_h
#define RootParser_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>

// Header file for the classes stored in the TTree if any.

class RootParser {
public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain

// Fixed size dimensions of array or collections stored in the TTree if any.

   // Declaration of leaf types
   Float_t         jetPt;
   Float_t         jetEta;
   Float_t         jetPhi;
   Float_t         jetMass;
   Float_t         jetGirth;
   Float_t         jetArea;
   Float_t         jetRawPt;
   Float_t         jetRawMass;
   UInt_t          jetLooseID;
   UInt_t          jetTightID;
   Float_t         jetJES;
   UInt_t          jetGenMatch;
   Int_t           physFlav;
   Int_t           isPhysUDS;
   Int_t           isPhysG;
   Int_t           isPhysOther;
   UInt_t          jetChargedHadronMult;
   UInt_t          jetNeutralHadronMult;
   UInt_t          jetChargedMult;
   UInt_t          jetNeutralMult;
   Int_t           jetMult;
   Float_t         jetQGl;
   Float_t         QG_ptD;
   Float_t         QG_axis2;
   Int_t           QG_mult;
   UInt_t          eventJetMult;
   Int_t           jetPtOrder;
   Float_t         dPhiJetsLO;
   Float_t         dEtaJetsLO;
   Float_t         alpha;
   Float_t         genJetPt;
   Float_t         genJetMass;
   Float_t         genJetEta;
   Float_t         genJetPhi;
   Float_t         genJetDeltaR;
   Int_t           nPF;
   Float_t         PF_pT[445];   //[nPF]
   Float_t         PF_dR[445];   //[nPF]
   Float_t         PF_dTheta[445];   //[nPF]
   Float_t         PF_dPhi[445];   //[nPF]
   Float_t         PF_dEta[445];   //[nPF]
   Float_t         PF_mass[445];   //[nPF]
   Int_t           PF_id[445];   //[nPF]
   Int_t           PF_fromAK5Jet[445];   //[nPF]
   Int_t           PF_fromPV[445];   //[nPF]
   Int_t           nGenJetPF;
   Float_t         genJetPF_pT[162];   //[nGenJetPF]
   Float_t         genJetPF_dR[162];   //[nGenJetPF]
   Float_t         genJetPF_dTheta[162];   //[nGenJetPF]
   Float_t         genJetPF_mass[162];   //[nGenJetPF]
   Int_t           genJetPF_id[162];   //[nGenJetPF]
   UInt_t          run;
   ULong64_t       event;
   UInt_t          lumi;
   Float_t         pthat;
   Float_t         eventWeight;
   Float_t         rho;
   UInt_t          PV_npvsGood;
   UInt_t          Pileup_nPU;
   Float_t         Pileup_nTrueInt;

   // List of branches
   TBranch        *b_jetPt;   //!
   TBranch        *b_jetEta;   //!
   TBranch        *b_jetPhi;   //!
   TBranch        *b_jetMass;   //!
   TBranch        *b_jetGirth;   //!
   TBranch        *b_jetArea;   //!
   TBranch        *b_jetRawPt;   //!
   TBranch        *b_jetRawMass;   //!
   TBranch        *b_jetLooseID;   //!
   TBranch        *b_jetTightID;   //!
   TBranch        *b_jetJES;   //!
   TBranch        *b_jetGenMatch;   //!
   TBranch        *b_physFlav;   //!
   TBranch        *b_isPhysUDS;   //!
   TBranch        *b_isPhysG;   //!
   TBranch        *b_isPhysOther;   //!
   TBranch        *b_jetChargedHadronMult;   //!
   TBranch        *b_jetNeutralHadronMult;   //!
   TBranch        *b_jetChargedMult;   //!
   TBranch        *b_jetNeutralMult;   //!
   TBranch        *b_jetMult;   //!
   TBranch        *b_jetQGl;   //!
   TBranch        *b_QG_ptD;   //!
   TBranch        *b_QG_axis2;   //!
   TBranch        *b_QG_mult;   //!
   TBranch        *b_eventJetMult;   //!
   TBranch        *b_jetPtOrder;   //!
   TBranch        *b_dPhiJetsLO;   //!
   TBranch        *b_dEtaJetsLO;   //!
   TBranch        *b_alpha;   //!
   TBranch        *b_genJetPt;   //!
   TBranch        *b_genJetMass;   //!
   TBranch        *b_genJetEta;   //!
   TBranch        *b_genJetPhi;   //!
   TBranch        *b_genJetDeltaR;   //!
   TBranch        *b_nPF;   //!
   TBranch        *b_PF_pT;   //!
   TBranch        *b_PF_dR;   //!
   TBranch        *b_PF_dTheta;   //!
   TBranch        *b_PF_dPhi;   //!
   TBranch        *b_PF_dEta;   //!
   TBranch        *b_PF_mass;   //!
   TBranch        *b_PF_id;   //!
   TBranch        *b_PF_fromAK5Jet;   //!
   TBranch        *b_PF_fromPV;   //!
   TBranch        *b_nGenJetPF;   //!
   TBranch        *b_genJetPF_pT;   //!
   TBranch        *b_genJetPF_dR;   //!
   TBranch        *b_genJetPF_dTheta;   //!
   TBranch        *b_genJetPF_mass;   //!
   TBranch        *b_genJetPF_id;   //!
   TBranch        *b_run;   //!
   TBranch        *b_event;   //!
   TBranch        *b_lumi;   //!
   TBranch        *b_pthat;   //!
   TBranch        *b_eventWeight;   //!
   TBranch        *b_rho;   //!
   TBranch        *b_PV_npvsGood;   //!
   TBranch        *b_Pileup_nPU;   //!
   TBranch        *b_Pileup_nTrueInt;   //!

   RootParser(TTree *tree=0);
   virtual ~RootParser();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop();
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
};

#endif

#ifdef RootParser_cxx
RootParser::RootParser(TTree *tree) : fChain(0) 
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("CMSOpenDataJets_MC_8TeV_500K.root");
      if (!f || !f->IsOpen()) {
         f = new TFile("CMSOpenDataJets_MC_8TeV_500K.root");
      }
      f->GetObject("OpenDataTree",tree);

   }
   Init(tree);
}

RootParser::~RootParser()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t RootParser::GetEntry(Long64_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Long64_t RootParser::LoadTree(Long64_t entry)
{
// Set the environment to read one entry
   if (!fChain) return -5;
   Long64_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (fChain->GetTreeNumber() != fCurrent) {
      fCurrent = fChain->GetTreeNumber();
      Notify();
   }
   return centry;
}

void RootParser::Init(TTree *tree)
{
   // The Init() function is called when the selector needs to initialize
   // a new tree or chain. Typically here the branch addresses and branch
   // pointers of the tree will be set.
   // It is normally not necessary to make changes to the generated
   // code, but the routine can be extended by the user if needed.
   // Init() will be called many times when running on PROOF
   // (once per file to be processed).

   // Set branch addresses and branch pointers
   if (!tree) return;
   fChain = tree;
   fCurrent = -1;
   fChain->SetMakeClass(1);

   fChain->SetBranchAddress("jetPt", &jetPt, &b_jetPt);
   fChain->SetBranchAddress("jetEta", &jetEta, &b_jetEta);
   fChain->SetBranchAddress("jetPhi", &jetPhi, &b_jetPhi);
   fChain->SetBranchAddress("jetMass", &jetMass, &b_jetMass);
   fChain->SetBranchAddress("jetGirth", &jetGirth, &b_jetGirth);
   fChain->SetBranchAddress("jetArea", &jetArea, &b_jetArea);
   fChain->SetBranchAddress("jetRawPt", &jetRawPt, &b_jetRawPt);
   fChain->SetBranchAddress("jetRawMass", &jetRawMass, &b_jetRawMass);
   fChain->SetBranchAddress("jetLooseID", &jetLooseID, &b_jetLooseID);
   fChain->SetBranchAddress("jetTightID", &jetTightID, &b_jetTightID);
   fChain->SetBranchAddress("jetJES", &jetJES, &b_jetJES);
   fChain->SetBranchAddress("jetGenMatch", &jetGenMatch, &b_jetGenMatch);
   fChain->SetBranchAddress("physFlav", &physFlav, &b_physFlav);
   fChain->SetBranchAddress("isPhysUDS", &isPhysUDS, &b_isPhysUDS);
   fChain->SetBranchAddress("isPhysG", &isPhysG, &b_isPhysG);
   fChain->SetBranchAddress("isPhysOther", &isPhysOther, &b_isPhysOther);
   fChain->SetBranchAddress("jetChargedHadronMult", &jetChargedHadronMult, &b_jetChargedHadronMult);
   fChain->SetBranchAddress("jetNeutralHadronMult", &jetNeutralHadronMult, &b_jetNeutralHadronMult);
   fChain->SetBranchAddress("jetChargedMult", &jetChargedMult, &b_jetChargedMult);
   fChain->SetBranchAddress("jetNeutralMult", &jetNeutralMult, &b_jetNeutralMult);
   fChain->SetBranchAddress("jetMult", &jetMult, &b_jetMult);
   fChain->SetBranchAddress("jetQGl", &jetQGl, &b_jetQGl);
   fChain->SetBranchAddress("QG_ptD", &QG_ptD, &b_QG_ptD);
   fChain->SetBranchAddress("QG_axis2", &QG_axis2, &b_QG_axis2);
   fChain->SetBranchAddress("QG_mult", &QG_mult, &b_QG_mult);
   fChain->SetBranchAddress("eventJetMult", &eventJetMult, &b_eventJetMult);
   fChain->SetBranchAddress("jetPtOrder", &jetPtOrder, &b_jetPtOrder);
   fChain->SetBranchAddress("dPhiJetsLO", &dPhiJetsLO, &b_dPhiJetsLO);
   fChain->SetBranchAddress("dEtaJetsLO", &dEtaJetsLO, &b_dEtaJetsLO);
   fChain->SetBranchAddress("alpha", &alpha, &b_alpha);
   fChain->SetBranchAddress("genJetPt", &genJetPt, &b_genJetPt);
   fChain->SetBranchAddress("genJetMass", &genJetMass, &b_genJetMass);
   fChain->SetBranchAddress("genJetEta", &genJetEta, &b_genJetEta);
   fChain->SetBranchAddress("genJetPhi", &genJetPhi, &b_genJetPhi);
   fChain->SetBranchAddress("genJetDeltaR", &genJetDeltaR, &b_genJetDeltaR);
   fChain->SetBranchAddress("nPF", &nPF, &b_nPF);
   fChain->SetBranchAddress("PF_pT", PF_pT, &b_PF_pT);
   fChain->SetBranchAddress("PF_dR", PF_dR, &b_PF_dR);
   fChain->SetBranchAddress("PF_dTheta", PF_dTheta, &b_PF_dTheta);
   fChain->SetBranchAddress("PF_dPhi", PF_dPhi, &b_PF_dPhi);
   fChain->SetBranchAddress("PF_dEta", PF_dEta, &b_PF_dEta);
   fChain->SetBranchAddress("PF_mass", PF_mass, &b_PF_mass);
   fChain->SetBranchAddress("PF_id", PF_id, &b_PF_id);
   fChain->SetBranchAddress("PF_fromAK5Jet", PF_fromAK5Jet, &b_PF_fromAK5Jet);
   fChain->SetBranchAddress("PF_fromPV", PF_fromPV, &b_PF_fromPV);
   fChain->SetBranchAddress("nGenJetPF", &nGenJetPF, &b_nGenJetPF);
   fChain->SetBranchAddress("genJetPF_pT", genJetPF_pT, &b_genJetPF_pT);
   fChain->SetBranchAddress("genJetPF_dR", genJetPF_dR, &b_genJetPF_dR);
   fChain->SetBranchAddress("genJetPF_dTheta", genJetPF_dTheta, &b_genJetPF_dTheta);
   fChain->SetBranchAddress("genJetPF_mass", genJetPF_mass, &b_genJetPF_mass);
   fChain->SetBranchAddress("genJetPF_id", genJetPF_id, &b_genJetPF_id);
   fChain->SetBranchAddress("run", &run, &b_run);
   fChain->SetBranchAddress("event", &event, &b_event);
   fChain->SetBranchAddress("lumi", &lumi, &b_lumi);
   fChain->SetBranchAddress("pthat", &pthat, &b_pthat);
   fChain->SetBranchAddress("eventWeight", &eventWeight, &b_eventWeight);
   fChain->SetBranchAddress("rho", &rho, &b_rho);
   fChain->SetBranchAddress("PV_npvsGood", &PV_npvsGood, &b_PV_npvsGood);
   fChain->SetBranchAddress("Pileup_nPU", &Pileup_nPU, &b_Pileup_nPU);
   fChain->SetBranchAddress("Pileup_nTrueInt", &Pileup_nTrueInt, &b_Pileup_nTrueInt);
   Notify();
}

Bool_t RootParser::Notify()
{
   // The Notify() function is called when a new file is opened. This
   // can be either for a new TTree in a TChain or when when a new TTree
   // is started when using PROOF. It is normally not necessary to make changes
   // to the generated code, but the routine can be extended by the
   // user if needed. The return value is currently not used.
   Loop();
   return kTRUE;
}

void RootParser::Show(Long64_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t RootParser::Cut(Long64_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}
#endif // #ifdef RootParser_cxx

#include "ParticlesStructureTester.h"
#include "TreeInitialiser2.h"
#include <boost/property_tree/ptree.hpp>
#include <iostream>
#include <string>
#include "ParticlesStructure.h"

ParticlesStructureTester::ParticlesStructureTester()
{	
		
}

ParticlesStructureTester::~ParticlesStructureTester()
{
}
void ParticlesStructureTester::test()
{
	
		std::cout<< "starting: loading tree\n";
		
		TreeInitialiser2 treeLoader;
		
		treeLoader.loadJSONTree("tree.json");
		
		std::cout<<"tree loaded, feeding structure" << std::endl;
		
		ParticlesStructure tester(*(treeLoader.getTreePtr()));
		
		std::cout<<"going to output, press enter";
		std::string temp;
		std::getline(std::cin,onenotetemp);
		
		for (auto it = tester.getXposArray().begin(); it!=tester.getXposArray().end(); it++ )
			std::cout<< " " << *it;
		
		std::cout<< "done" << std::endl;
}

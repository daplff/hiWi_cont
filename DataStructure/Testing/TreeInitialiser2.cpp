#include "TreeInitialiser2.h"
#include <boost/property_tree/ptree.hpp>
#include <fstream>
#include <boost/property_tree/json_parser.hpp>

TreeInitialiser2::TreeInitialiser2()
{
	treePtr = std::shared_ptr<boost::property_tree::ptree>(new boost::property_tree::ptree);
}

std::shared_ptr<boost::property_tree::ptree> TreeInitialiser2::getTreePtr() {
	return treePtr;
}

TreeInitialiser2::~TreeInitialiser2()
{
}

void TreeInitialiser2::printJSONTree(std::string filePath) {

	boost::property_tree::json_parser::write_json(filePath,*treePtr);

}

void TreeInitialiser2::loadJSONTree(std::string filePath) {

	boost::property_tree::json_parser::read_json(filePath, *treePtr);

}




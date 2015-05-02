#ifndef TREEINITIALISER2_H
#define TREEINITIALISER2_H

#include <boost/property_tree/ptree_fwd.hpp>
#include <memory>


class TreeInitialiser2
{
public:
	TreeInitialiser2();
	~TreeInitialiser2();
	std::shared_ptr<boost::property_tree::ptree> getTreePtr();
	void printJSONTree(std::string filePath);
	void loadJSONTree(std::string filePath);

private:
	std::shared_ptr<boost::property_tree::ptree> treePtr;


};

#endif // TREEINITIALISER2_H

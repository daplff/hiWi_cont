#ifndef PARTICLESSTRUCTURE_H
#define PARTICLESSTRUCTURE_H

#include <vector>
#include <boost/property_tree/ptree_fwd.hpp>

class ParticlesStructure
{
public:
    ParticlesStructure();
    ~ParticlesStructure();
    ParticlesStructure(const boost::property_tree::ptree& input);

    std::vector<double>& getXposArray()
	{
	return xposArray;
	}
    std::vector<double>& getYposArray()
	{
	return yposArray;
	}

private:
    std::vector<double> xposArray;
    std::vector<double> yposArray;
    std::vector<double> xvelArray;
    std::vector<double> yvelArray;
    std::vector<double> areaArray;
    std::vector<double> smoothlenArray;
};

#endif // PARTICLESSTRUCTURE_H

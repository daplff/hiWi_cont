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


    void resizeArrays(size_t newSize);

    std::vector<double>& getAreaArray() {
		return areaArray;
	}

	std::vector<short>& getFlagArray() {
		return flagArray;
	}

	std::vector<double>& getSmoothlenArray() {
		return smoothlenArray;
	}

	std::vector<double>& getSurfElevationArray() {
		return surfElevationArray;
	}

	std::vector<double>& getXvelArray() {
		return xvelArray;
	}

	std::vector<double>& getYvelArray() {
		return yvelArray;
	}

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
    std::vector<double> surfElevationArray;
    std::vector<double> areaArray;
    std::vector<double> smoothlenArray;
    std::vector<short> flagArray;

};

#endif // PARTICLESSTRUCTURE_H

#include "stdafx.h"
#include "Test.h"
#include <boost\math\special_functions\beta.hpp>
#include <boost\math\distributions\normal.hpp>

double Test::InverseIncompleteBeta(double a, double b, double x)
{
	return boost::math::ibeta_inv(a, b, x);
}

double Test::NormalDistribution(double m, double s, double quantile)
{
	boost::math::normal sol = boost::math::normal::normal_distribution(m, s);
	return boost::math::quantile(sol, quantile);
}

// wordpuzzle.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include <iostream>
#include <fstream>
#include <map>
#include <vector>
#include <iterator>
#include <algorithm>

#include <boost/config.hpp>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/dijkstra_shortest_paths.hpp>
#include <boost/property_map/property_map.hpp>

using namespace boost;
using namespace std;

struct fixed_weight_pmap 
{
};

template <class K>
inline int get(const fixed_weight_pmap& pa, const K& k) 
{ 
	return 1; 
}

template<>
struct property_traits<fixed_weight_pmap>
{
	typedef int value_type;
};

static bool neighbours(const string& s1, const string& s2)
{
	const size_t length = s1.length();
	if (length != s2.length())
		return false;

	int diff = 0;
	for (auto i = 0U; i < length && diff <= 1; ++i)
	{
		if (s1[i] != s2[i])
			++diff;
	}

	return diff == 1;
}

int main(int, char *[])
{
	typedef adjacency_list<listS, vecS, undirectedS, no_property, property<edge_weight_t, int>> graph_t;
	typedef graph_traits<graph_t>::vertex_descriptor vertex_descriptor_t;
	typedef graph_traits<graph_t>::edge_descriptor edge_descriptor_t;
	
	vector<string> dictionary(
		istream_iterator<string>(ifstream("dictionary.txt")), 
		istream_iterator<string>());
	dictionary.push_back("ruby");

	map<string, int> word2index;
	for (auto i = 0U; i < dictionary.size(); ++i)
	{
		string w = dictionary[i];
		word2index[w] = i;
	}

	graph_t g(dictionary.size());
	for (auto i = 0U; i < dictionary.size(); ++i) 
	{
		if (i % 1000 == 0) {
			cout << i << " (" << dictionary.size() << ")" << endl;
		}
		for (auto j = i + 1; j < dictionary.size(); ++j)
		{
			if (neighbours(dictionary[i], dictionary[j]))
			{
				add_edge(i, j, g);
			}
		}
	}

	std::vector<vertex_descriptor_t> predecessors(num_vertices(g));
	std::vector<int> distances(num_vertices(g));

	while (true)
	{
		string source, target;
		cout << "source:" << endl;
		cin >> source;
		cout << "target:" << endl;
		cin >> target;

		auto sourceIndex = word2index[source];
		auto targetIndex = word2index[target];

		dijkstra_shortest_paths(
			g,
			sourceIndex,
			predecessor_map(&predecessors[0]).
			distance_map(&distances[0]).
			weight_map(fixed_weight_pmap())
		);

		list<string> path;
		int index = targetIndex;
		while (index != sourceIndex)
		{		
			path.push_back(dictionary[index]);
			index = predecessors[index];
		}

		path.push_back(dictionary[index]);
		path.reverse();

		cout << "path:" << endl;
		copy(path.begin(), path.end(), ostream_iterator<string>(cout, " "));
		cout << endl;
	}

	return 0;
}


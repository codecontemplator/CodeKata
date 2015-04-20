#include <iostream>
#include <fstream>
#include <map>
#include <vector>
#include <iterator>
#include <algorithm>

#include <boost/config.hpp>
#include <boost/range/counting_range.hpp>
#include <boost/property_map/property_map.hpp>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/dijkstra_shortest_paths.hpp>

struct fixed_weight_pmap { };

template <class K> inline int get(const fixed_weight_pmap& pa, const K& k)  { return 1; }

namespace boost { template<> struct property_traits<fixed_weight_pmap> { typedef int value_type; }; }

bool neighbours(const std::string& s1, const std::string& s2)
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

struct context_t
{
	typedef boost::adjacency_list<boost::listS, boost::vecS, boost::undirectedS> graph_t;
	typedef std::vector<std::string> dictionary_t;
	typedef dictionary_t::size_type index_t;
	typedef std::map<std::string, index_t> index_lookup_t;

	graph_t g;
	dictionary_t dictionary;
	index_lookup_t word2index;
};

void initialize(context_t& ctx)
{
	using namespace boost;
	using namespace std;

	typedef context_t::graph_t graph_t;
	typedef context_t::dictionary_t dictionary_t;
	typedef context_t::index_lookup_t index_lookup_t;
	typedef context_t::index_t index_t;

	dictionary_t dictionary(
		istream_iterator<string>(ifstream("dictionary.txt")),
		istream_iterator<string>());

	index_lookup_t word2index;
	transform(
		dictionary.begin(), dictionary.end(),
		counting_range<index_t>(0, dictionary.size()).begin(),
		inserter(word2index, word2index.begin()),
		make_pair<const string&, const index_t&>);

	graph_t g(dictionary.size());
	for (auto i = 0U; i < dictionary.size(); ++i)
	{
		if (i % 5000 == 0) { cout << i << " (" << dictionary.size() << ")" << endl; }
		for (auto j = i + 1; j < dictionary.size(); ++j)
		{
			if (neighbours(dictionary[i], dictionary[j]))
				add_edge(i, j, g);
		}
	}

	ctx.dictionary.swap(dictionary);
	ctx.word2index.swap(word2index);
	ctx.g.swap(g);
}

std::list<std::string> wordchain(context_t& ctx, const std::string& source, const std::string& target)
{
	using namespace boost;
	using namespace std;

	typedef graph_traits<context_t::graph_t>::vertex_descriptor vertex_descriptor_t;
	typedef graph_traits<context_t::graph_t>::edge_descriptor edge_descriptor_t;

	const auto sourceIndex = ctx.word2index[source];
	const auto targetIndex = ctx.word2index[target];

	vector<vertex_descriptor_t> predecessors(num_vertices(ctx.g));
	vector<int> distances(num_vertices(ctx.g));

	dijkstra_shortest_paths(
		ctx.g,
		sourceIndex,
		predecessor_map(&predecessors[0]).
		distance_map(&distances[0]).
		weight_map(fixed_weight_pmap())
		);

	list<string> path;
	auto index = targetIndex;
	while (index != sourceIndex)
	{
		path.push_back(ctx.dictionary[index]);
		index = predecessors[index];
	}

	path.push_back(ctx.dictionary[index]);
	path.reverse();
	return path;
}

int main(int, char *[])
{
	using namespace std;

	cout << "initializing..." << endl;
	context_t ctx;
	initialize(ctx);
	cout << "done!" << endl;

	while (true)
	{
		string source, target;
		cout << "source: ";
		cin >> source;
		cout << "target: ";
		cin >> target;

		if (source.length() == target.length())
		{
			auto path = wordchain(ctx, source, target);
			cout << "path: ";
			copy(path.begin(), path.end(), ostream_iterator<string>(cout, " "));
			cout << endl;
		}
		else
		{
			cout << "source and target must be of same length" << endl;
		}
	}

	return 0;
}


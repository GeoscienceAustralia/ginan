#define BOOST_TEST_MODULE trace_file_cache_tests
#include <boost/iostreams/stream.hpp>
#include <boost/test/unit_test.hpp>
#include <chrono>
#include <filesystem>
#include <unordered_set>

#include "common/trace.hpp"

thread_local boost::iostreams::stream<boost::iostreams::null_sink> nullStream{
    boost::iostreams::null_sink{}
};

namespace
{
std::filesystem::path makeTempDir()
{
    auto suffix = std::chrono::steady_clock::now().time_since_epoch().count();
    auto path   = std::filesystem::temp_directory_path() /
                  ("ginan_trace_cache_test_" + std::to_string(suffix));

    std::filesystem::create_directories(path);
    return path;
}
}  // namespace

BOOST_AUTO_TEST_CASE(retain_trace_files_prunes_rotated_trace_streams)
{
    auto tempDir = makeTempDir();
    auto fileA   = (tempDir / "trace-a.trace").string();
    auto fileB   = (tempDir / "trace-b.trace").string();

    retainTraceFiles({});

    {
        auto traceA = getTraceFile(fileA, string("A"));
        traceA << "first\n";
        traceA.flush();
    }

    auto traceB = getTraceFile(fileB, string("B"));
    traceB << "second\n";
    traceB.flush();

    BOOST_REQUIRE(traceFileCache().find(fileA) != traceFileCache().end());
    BOOST_REQUIRE(traceFileCache().find(fileB) != traceFileCache().end());

    retainTraceFiles({fileB});

    BOOST_CHECK(traceFileCache().find(fileA) == traceFileCache().end());
    BOOST_CHECK(traceFileCache().find(fileB) != traceFileCache().end());

    traceB.flush();
    retainTraceFiles({});
    std::filesystem::remove_all(tempDir);
}

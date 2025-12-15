// #pragma GCC optimize ("O0")

#include "common/fileLog.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/json.hpp>
#include <boost/log/sinks/sync_frontend.hpp>
#include <boost/log/trivial.hpp>
#include <chrono>
#include <ctime>

namespace sinks = boost::log::sinks;

using LogSink = sinks::synchronous_sink<FileLog>;

string FileLog::path_log;

void FileLog::consume(
    boost::log::record_view const& rec,
    sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type const&
        log_string
)
{
    string mess = log_string.c_str();
    boost::erase_all(mess, "\n");
    if (mess.empty())
        return;

    int  logLevel = 2;
    auto attrs    = rec.attribute_values();
    auto sev      = attrs[boost::log::trivial::severity].get();
    switch (sev)
    {
        case boost::log::trivial::trace:
            logLevel = 5;
            break;
        case boost::log::trivial::debug:
            logLevel = 4;
            break;
        case boost::log::trivial::info:
            logLevel = 3;
            break;
        case boost::log::trivial::warning:
            logLevel = 2;
            break;
        case boost::log::trivial::error:
            logLevel = 1;
            break;
        case boost::log::trivial::fatal:
            logLevel = 0;
            break;
    }

    std::ofstream logStream(FileLog::path_log, std::ofstream::app);

    if (!logStream)
        return;

    GTime time = timeGet();

    boost::json::object doc = {};
    doc["label"]            = "message";
    doc["Time"]             = time.to_string();
    doc["level"]            = logLevel;
    doc["str"]              = mess;

    if (json)
        logStream << boost::json::serialize(doc) << "\n";
    else
        logStream << logLevel << ": " << mess << "\n";
}

void addFileLog(bool json)
{
    // Construct the sink

    boost::shared_ptr<LogSink> logSink = boost::make_shared<LogSink>();

    logSink->locked_backend()->json = json;

    // Register the sink in the logging core
    boost::log::core::get()->add_sink(logSink);
}

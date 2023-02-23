//
// Created by zcobell on 2/20/23.
//

#include "report.h"

#include <fstream>
#include <iomanip>
#include <iostream>

#include "stack.h"
#include "stackimpl.h"

#ifdef SMARTSTACK_USE_LIBFMT
#include "fmt/core.h"
#else
#include <format>
#endif

using namespace SmartStack;

void Report::printTimingReport(const TimeUnits units, const SortType st,
                               const SortOrder so) {
  const auto report = this->generateTableTimingReport(units, st, so);
  for (auto &s : report) {
    std::cout << s << "\n";
  }
}

void Report::saveTimingReport(const std::string &filename,
                              const TimeUnits units, const SortType st,
                              const SortOrder so, const OutputFormat of) {
  if (of == Report::OutputFormat::CSV) {
    this->saveTimingReportCSV(units, st, so, filename);
  } else {
    auto report = this->generateTableTimingReport(units, st, so);
    this->saveTimingReportTable(report, filename);
  }
}

std::vector<std::string> Report::generateTableTimingReport(
    const Report::TimeUnits unit, const SortType st, const SortOrder so) {
  auto sorted_functions = this->sortFunctions(st, so);
  auto sort_codes = SortCodes(st, so);
  auto units = Report::unitsString(unit);

  const size_t padsize = (this->maxNumFunctionChars(13) - 13) / 2;
  std::string pad = std::string(padsize + 1, ' ');
  std::string dashfn = std::string(2 * padsize + 16, '-');
  std::string fn = pad + " Function Name" + pad;

  auto header_bar = fmt::format(
      "|----------|{:s}|-------------|----------------------------|------------"
      "-------------------|----------------------------------|-----------------"
      "---------------------|",
      dashfn);

  std::vector<std::string> table;
  table.reserve(SmartStack::Stack::get()->functions().size() + 3);
  table.push_back(
      fmt::format("Function report for: {:s}", Stack::get()->sessionName()));
  table.push_back(header_bar);
  table.push_back(fmt::format(
      "|   Rank   |{:s}|  {:s} Calls  |  {:s} Local Duration {:s}   | {:s} "
      "Mean "
      "Local Duration {:s}  | {:s} Local + Child Duration {:s}  | {:s} Mean "
      "Local + Child Duration {:s} |",
      fn, sort_codes.calls, sort_codes.duration, units,
      sort_codes.mean_duration, units, sort_codes.total_duration, units,
      sort_codes.mean_total_duration, units));
  table.push_back(header_bar);

  size_t i = 0;
  for (auto &f : sorted_functions) {
    i++;
    auto line = this->getFunctionReportLine(i, f, unit, Report::Table);
    table.push_back(line);
  }
  table.push_back(header_bar);
  return table;
}

std::string Report::unitsString(Report::TimeUnits units, bool trim) {
  if (!trim) {
    switch (units) {
      case Microseconds:
        return "(us)";
      case Milliseconds:
        return "(ms)";
      case Seconds:
        return "(s) ";
      case Minutes:
        return "(m) ";
      case Hours:
        return "(h) ";
    }
  } else {
    switch (units) {
      case Microseconds:
        return "_us";
      case Milliseconds:
        return "_ms";
      case Seconds:
        return "_s";
      case Minutes:
        return "_m";
      case Hours:
        return "_h";
    }
  }
  return "?";
}

void Report::saveTimingReportCSV(const TimeUnits units, const SortType st,
                                 const SortOrder so,
                                 const std::string &filename) {
  std::ofstream f(filename);
  std::string unit = Report::unitsString(units, true);

  f << fmt::format(
      "Rank,Function,NumCalls,LocalTime{:s},MeanLocalTime{:"
      "s},LocalAndChildTime{:s},MeanLocalAndChildTime{:s}\n",
      unit, unit, unit, unit);

  size_t i = 0;
  for (auto &fn : Stack::get()->functions()) {
    i += 1;
    f << this->getFunctionReportLine(i, fn, units, Report::CSV) << "\n";
  }
  f.close();
}

void Report::saveTimingReportTable(const std::vector<std::string> &report,
                                   const std::string &filename) {
  std::ofstream output(filename);
  for (auto &s : report) {
    output << s << std::endl;
  }
  output.close();
}

std::string pad_right(const std::string &str, size_t s) {
  if (str.size() < s) {
    return str + std::string(s - str.size(), ' ');
  } else {
    return str;
  }
}

std::string pad_left(const std::string &str, size_t s) {
  if (str.size() < s) {
    return std::string(s - str.size(), ' ') + str;
  } else {
    return str;
  }
}

std::string Report::getFunctionReportLine(
    size_t i, detail::Function *f, const Report::TimeUnits &units,
    const Report::OutputFormat &format) const {
  if (units == Seconds || units == Hours || units == Minutes ||
      units == Milliseconds) {
    const double multiplier = [&]() {
      switch (units) {
        case Seconds:
          return 1e6;
        case Minutes:
          return 1e6 * 60.0;
        case Hours:
          return 1e6 * 60.0 * 60.0;
        case Milliseconds:
          return 1000.0;
        default:
          return 1.0;
      }
    }();

    const double ts =
        Report::convertTimeUnitsDouble(f->timer()->elapsed(), multiplier);
    const double mts =
        Report::convertTimeUnitsDouble(f->meanDuration(), multiplier);
    const double ats =
        Report::convertTimeUnitsDouble(f->timer()->globalElapsed(), multiplier);
    const double amts =
        Report::convertTimeUnitsDouble(f->meanGlobalDuration(), multiplier);

    if (format == Report::Table) {
      const auto name =
          pad_right(f->name(), SmartStack::Report::maxNumFunctionChars(13));
      return fmt::format(
          "| {:8d} | {:s} | {:11d} | {:26.9e} | {:29.9e} | {:32.9e} | {:36.9e} "
          "|",
          i, name, f->numCalls(), ts, mts, ats, amts);
    } else {
      return fmt::format("{:d},{:s},{:d},{:9.9e},{:9.9e},{:9.9e},{:9.9e}", i,
                         f->name(), f->numCalls(), ts, mts, ats, amts);
    }
  } else {
    if (format == Report::Table) {
      const auto name =
          pad_right(f->name(), SmartStack::Report::maxNumFunctionChars(13));
      return fmt::format(
          "| {:8d} | {:s} | {:11d} | {:26d} | {:29d} | {:32d} | {:36d} |", i,
          name, f->numCalls(), f->timer()->elapsed(), f->meanDuration(),
          f->timer()->globalElapsed(), f->meanGlobalDuration());
    } else {
      return fmt::format("{:d},{:s},{:d},{:d},{:d},{:d},{:d}", i, f->name(),
                         f->numCalls(), f->timer()->elapsed(),
                         f->meanDuration(), f->timer()->globalElapsed(),
                         f->meanGlobalDuration());
    }
  }
}

double Report::convertTimeUnitsDouble(const long long time,
                                      const double multiplier) {
  return static_cast<double>(time) / multiplier;
}

size_t Report::maxNumFunctionChars(size_t lowerLimit) {
  size_t mx = lowerLimit;
  for (auto &f : Stack::get()->functions()) {
    mx = std::max(mx, f->name().size());
  }
  if (mx % 2 == 0) return mx;
  return mx + 1;
}

std::vector<detail::Function *> Report::sortFunctions(const SortType &st,
                                                      const SortOrder &so) {
  auto functions = Stack::get()->functions();
  switch (st) {
    case Report::Time:
      std::sort(functions.begin(), functions.end(),
                [](detail::Function *a, detail::Function *b) {
                  return a->timer()->elapsed() < b->timer()->elapsed();
                });
      break;
    case Report::Calls:
      std::sort(functions.begin(), functions.end(),
                [](detail::Function *a, detail::Function *b) {
                  return a->numCalls() < b->numCalls();
                });
      break;
    case Report::MeanTime:
      std::sort(functions.begin(), functions.end(),
                [](detail::Function *a, detail::Function *b) {
                  return a->meanDuration() < b->meanDuration();
                });
      break;
    case Report::MeanTotalTime:
      std::sort(functions.begin(), functions.end(),
                [](detail::Function *a, detail::Function *b) {
                  return a->meanGlobalDuration() < b->meanGlobalDuration();
                });
      break;
    case Report::TotalTime:
      std::sort(functions.begin(), functions.end(),
                [](detail::Function *a, detail::Function *b) {
                  return a->timer()->globalElapsed() <
                         b->timer()->globalElapsed();
                });
      break;
    default:
      std::sort(functions.begin(), functions.end(),
                [](detail::Function *a, detail::Function *b) {
                  return a->timer()->elapsed() < b->timer()->elapsed();
                });
  }
  if (so == Descending) std::reverse(functions.begin(), functions.end());
  return functions;
}
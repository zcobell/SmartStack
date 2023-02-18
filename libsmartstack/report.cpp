//
// Created by Zach Cobell on 2/17/23.
//

#include "report.h"
#include <algorithm>
#include <fstream>
#include <iostream>

using namespace SmartStack;

Report::Report(const std::string &sessionName, const Report::TimeUnits &units,
               const Report::SortType &st, const Report::SortOrder &so)
    : m_sessionName(sessionName), m_reportUnits(units), m_sortOrder(so),
      m_sortType(st) {}

void Report::setReportUnits(const Report::TimeUnits &units) {
  m_reportUnits = units;
}

std::vector<Function *>
Report::sortFunctions(const std::vector<Function *> &functions) const {
  std::vector<Function *> sortedFunctions;
  sortedFunctions.reserve(functions.size());
  for (auto &f : functions) {
    sortedFunctions.push_back(f);
  }
  switch (m_sortType) {
  case Time:
    std::sort(sortedFunctions.begin(), sortedFunctions.end(),
              [](const Function *a, const Function *b) {
                return a->timer()->elapsed() > b->timer()->elapsed();
              });
    break;
  case Calls:
    std::sort(sortedFunctions.begin(), sortedFunctions.end(),
              [](const Function *a, const Function *b) {
                return a->numCalls() > b->numCalls();
              });
    break;
  case MeanTime:
    std::sort(sortedFunctions.begin(), sortedFunctions.end(),
              [](const Function *a, const Function *b) {
                return a->meanDuration() > b->meanDuration();
              });
    break;
  case MeanTotalTime:
    std::sort(sortedFunctions.begin(), sortedFunctions.end(),
              [](const Function *a, const Function *b) {
                return a->meanGlobalDuration() > b->meanGlobalDuration();
              });
    break;
  case TotalTime:
  default:
    std::sort(sortedFunctions.begin(), sortedFunctions.end(),
              [](const Function *a, const Function *b) {
                return a->timer()->globalElapsed() >
                       b->timer()->globalElapsed();
              });
  }

  if (m_sortOrder == Descending) {
    std::reverse(sortedFunctions.begin(), sortedFunctions.end());
  }

  return sortedFunctions;
}

void Report::getSortCodes(std::string &calls, std::string &duration,
                          std::string &meanDuration, std::string &totalDuration,
                          std::string &meanTotalDuration, const SortType &st,
                          const SortOrder &so) const {
  constexpr std::string_view upArrow = "[^]";
  constexpr std::string_view downArrow = "[v]";
  constexpr std::string_view blank = "[-]";

  if (st == SortType::Time) {
    calls = blank;
    meanDuration = blank;
    totalDuration = blank;
    meanTotalDuration = blank;
    duration = so == Ascending ? downArrow : upArrow;
  } else if (st == SortType::Calls) {
    calls = so == Ascending ? downArrow : upArrow;
    meanDuration = blank;
    duration = blank;
    totalDuration = blank;
    meanTotalDuration = blank;
  } else if (st == SortType::MeanTime) {
    calls = blank;
    totalDuration = blank;
    meanTotalDuration = blank;
    meanDuration = so == Ascending ? downArrow : upArrow;
    duration = blank;
  } else if (st == SortType::TotalTime) {
    calls = blank;
    meanDuration = blank;
    meanTotalDuration = blank;
    totalDuration = so == Ascending ? downArrow : upArrow;
    duration = blank;
  } else if (st == SortType::MeanTotalTime) {
    calls = blank;
    meanDuration = blank;
    totalDuration = blank;
    meanTotalDuration = so == Ascending ? downArrow : upArrow;
    duration = blank;
  }
}

std::vector<std::string>
Report::generateTableTimingReport(const std::vector<Function *> &functions,
                                  const SortType &st,
                                  const SortOrder &so) const {
  auto sorted_functions = this->sortFunctions(functions);
  std::string callCode, durationCode, meanDurationCode, totalDurationCode,
      meanTotalDurationCode;
  this->getSortCodes(callCode, durationCode, meanDurationCode,
                     totalDurationCode, meanTotalDurationCode, st, so);
  std::string unit = Report::unitsString(this->m_reportUnits, false);

  std::vector<std::string> table;

  size_t padsize = (this->maxNumFunctionChars(sorted_functions, 13) - 13) / 2;
  std::string pad = std::string(padsize + 1, ' ');

  std::string dashfn = std::string(2 * padsize + 16, '-');
  std::string fn = pad + " Function Name" + pad;
  std::string headerbar =
      "|----------|" + dashfn +
      "|-------------|----------------------------|----------------------------"
      "---|----------------------------------|---------------------------------"
      "-----|";

  table.push_back("Function report for: " + this->m_sessionName);
  table.push_back(headerbar);
  table.push_back("|   Rank   |" + fn + "| " + callCode + " Calls   |  " +
                  durationCode + "  Local Duration  " + unit + " | " +
                  meanDurationCode + "  Mean Local Duration " + unit + " |  " +
                  totalDurationCode + " Local + Child Duration " + unit +
                  " | " + meanTotalDurationCode +
                  " Mean Local + Child Duration " + unit + " |");
  table.push_back(headerbar);

  size_t i = 0;
  auto max_function_name_length =
      this->maxNumFunctionChars(sorted_functions, 13);
  for (auto &f : sorted_functions) {
    i++;
    std::string line = this->getFunctionReportLine(
        i, f, max_function_name_length, this->m_reportUnits, Report::Table);
    table.push_back(line);
  }
  table.push_back(headerbar);
  return table;
}

std::string Report::unitsString(const Report::TimeUnits &units,
                                bool trim) const {
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

void Report::printTimingReport(const std::vector<Function *> &functions) const {
  auto report = this->generateTableTimingReport(functions, this->m_sortType,
                                                this->m_sortOrder);
  for (const auto &s : report) {
    std::cout << s << std::endl;
  }
}

void Report::saveCsvTimingReport(const std::vector<Function *> &functions,
                                 const std::string &filename) const {
  std::ofstream f(filename);
  auto u = this->m_reportUnits;
  auto unit = this->unitsString(this->m_reportUnits, true);
  f << "Rank,Function,NumCalls,LocalTime" + unit + ",MeanLocalTime" + unit +
           ",LocalAndChildTime" + unit + "," + "MeanLocalAndChildTime" + unit
    << "\n";
  size_t i = 0;
  auto sorted_functions = this->sortFunctions(functions);
  auto max_function_name = this->maxNumFunctionChars(sorted_functions, 13);
  for (auto &fn : functions) {
    i += 1;
    f << this->getFunctionReportLine(i, fn, max_function_name, u, Report::CSV)
      << "\n";
  }
  f.close();
}

void Report::saveTableTimingReport(const std::vector<Function *> &functions,
                                   const std::string &filename) const {
  auto report_data = this->generateTableTimingReport(
      functions, this->m_sortType, this->m_sortOrder);
  std::ofstream output(filename);
  for (const auto &s : report_data) {
    output << s << "\n";
  }
  output.close();
}

std::string Report::getFunctionReportLine(
    size_t i, Function *f, const size_t function_name_max_length,
    const Report::TimeUnits &units, const Report::OutputFormat &format) const {
  char line[400];
  if (units == Seconds || units == Hours || units == Minutes ||
      units == Milliseconds) {
    const double multiplier = [](const Report::TimeUnits &units) {
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
    }(units);

    double ts =
        Report::convertTimeUnitsDouble(f->timer()->elapsed(), multiplier);
    double mts = Report::convertTimeUnitsDouble(f->meanDuration(), multiplier);
    double ats =
        Report::convertTimeUnitsDouble(f->timer()->globalElapsed(), multiplier);
    double amts =
        Report::convertTimeUnitsDouble(f->meanGlobalDuration(), multiplier);

    if (format == Report::Table) {
      std::string formatLine =
          std::string() + "| %8zu | %" +
          formatStringChar(function_name_max_length) +
          "s | %11lld |            %9.9e |               %9.9e |       "
          "           %9.9e |                      %9.9e |";

      snprintf(line, 400, formatLine.c_str(), i, f->name().c_str(),
               f->numCalls(), ts, mts, ats, amts);
    } else {
      snprintf(line, 400, "%zu,%s,%lld,%9.9e,%9.9e,%9.9e,%9.9e", i,
               f->name().c_str(), f->numCalls(), ts, mts, ats, amts);
    }
  } else {
    if (format == Report::Table) {
      std::string formatLine =
          std::string() + "| %8zu | %" +
          formatStringChar(function_name_max_length) +
          "s | %11lld |         %18lld |            %18lld "
          "|               %18lld |  "
          "                 %18lld |";
      snprintf(line, 400, formatLine.c_str(), i, f->name().c_str(),
               f->numCalls(), f->timer()->elapsed(), f->meanDuration(),
               f->timer()->globalElapsed(), f->meanGlobalDuration());
    } else {
      snprintf(line, 400, "%zu,%s,%lld,%lld,%lld,%lld,%lld", i,
               f->name().c_str(), f->numCalls(), f->timer()->elapsed(),
               f->meanDuration(), f->timer()->globalElapsed(),
               f->meanGlobalDuration());
    }
  }
  return {line};
}

double Report::convertTimeUnitsDouble(const size_t time,
                                      const double multiplier) {
  auto td = static_cast<double>(time);
  auto t = td / multiplier;
  return t + (td - t) / multiplier;
}

size_t Report::maxNumFunctionChars(const std::vector<Function *> &functions,
                                   size_t lowerLimit) {
  size_t mx = lowerLimit;
  for (auto &f : functions) {
    mx = std::max(mx, f->name().size());
  }
  if (mx % 2 == 0)
    return mx;
  return mx + 1;
}

std::string Report::formatStringChar(size_t n) {
  char line[20];
  if (n == 13)
    snprintf(line, 20, "%zu", n + 1);
  else
    snprintf(line, 20, "%zu", n);
  return {line};
}

void Report::setSortType(const Report::SortType &st) { m_sortType = st; }

void Report::setSortOrder(const SmartStack::Report::SortOrder &so) {
  m_sortOrder = so;
}

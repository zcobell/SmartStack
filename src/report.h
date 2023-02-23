//
// Created by zcobell on 2/20/23.
//

#ifndef SMARTSTACK_REPORT_H
#define SMARTSTACK_REPORT_H

#include <string>
#include <vector>

#include "function.h"

namespace SmartStack {

class Report {
 public:
  enum SortOrder { Ascending, Descending };
  enum SortType { Time, MeanTime, TotalTime, MeanTotalTime, Calls };
  enum TimeUnits { Microseconds, Milliseconds, Seconds, Minutes, Hours };
  enum OutputFormat { Table, CSV };

  Report() = default;

  void printTimingReport(TimeUnits units, SortType st, SortOrder so);

  void saveTimingReport(const std::string &filename, TimeUnits units,
                        SortType st, SortOrder so, OutputFormat of);

 private:
  void saveTimingReportCSV(TimeUnits units, SortType st, SortOrder so,
                           const std::string &filename);
  void saveTimingReportTable(const std::vector<std::string> &report,
                             const std::string &filename);

  static std::string unitsString(TimeUnits unit, bool trim = false);

  static std::vector<detail::Function *> sortFunctions(const SortType &st,
                                                       const SortOrder &so);

  struct SortCodes {
    inline static const std::string blank = "[-]";
    inline static const std::string upArrow = "[^]";
    inline static const std::string downArrow = "[v]";

    SortCodes(SortType sort, SortOrder order)
        : calls(blank),
          duration(blank),
          mean_duration(blank),
          total_duration(blank),
          mean_total_duration(blank) {
      switch (sort) {
        case Time:
          duration = getArrow(order);
          break;
        case MeanTime:
          mean_duration = getArrow(order);
          break;
        case TotalTime:
          total_duration = getArrow(order);
          break;
        case MeanTotalTime:
          mean_total_duration = getArrow(order);
          break;
        case Calls:
          calls = getArrow(order);
          break;
        default:
          break;
      }
    }

    std::string calls;
    std::string duration;
    std::string mean_duration;
    std::string total_duration;
    std::string mean_total_duration;

   private:
    std::string getArrow(SortOrder s) {
      return s == Ascending ? upArrow : downArrow;
    };
  };

  static double convertTimeUnitsDouble(long long time, double multiplier);

  static size_t maxNumFunctionChars(size_t lowerLimit = 0);

  std::string getFunctionReportLine(size_t i, detail::Function *f,
                                    const Report::TimeUnits &units,
                                    const Report::OutputFormat &format) const;

 private:
  std::vector<std::string> generateTableTimingReport(TimeUnits unit,
                                                     SortType st, SortOrder so);
};
}  // namespace SmartStack

#endif  // SMARTSTACK_REPORT_H

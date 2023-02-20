//
// Created by Zach Cobell on 2/17/23.
//

#ifndef SMARTSTACK_LIBSMARTSTACK_REPORT_H_
#define SMARTSTACK_LIBSMARTSTACK_REPORT_H_

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

  explicit Report(const std::string &sessionName,
                  const TimeUnits &units = Seconds, const SortType &st = Time,
                  const SortOrder &so = Descending);

  void setReportUnits(const TimeUnits &units);
  void setSortType(const SortType &st);
  void setSortOrder(const SortOrder &so);

  void printTimingReport(const std::vector<Function *> &functions) const;

  void saveTableTimingReport(const std::vector<Function *> &functions,
                             const std::string &filename) const;

  void saveCsvTimingReport(const std::vector<Function *> &functions,
                           const std::string &filename) const;

 private:
  [[nodiscard]] std::vector<Function *> sortFunctions(
      const std::vector<Function *> &functions) const;

  void getSortCodes(std::string &calls, std::string &duration,
                    std::string &meanDuration, std::string &totalDuration,
                    std::string &meanTotalDuration, const SortType &st,
                    const SortOrder &so) const;

  [[nodiscard]] std::vector<std::string> generateTableTimingReport(
      const std::vector<Function *> &functions, const SortType &st,
      const SortOrder &so) const;

  [[nodiscard]] std::string unitsString(const Report::TimeUnits &units,
                                        bool trim) const;

  [[nodiscard]] std::string getFunctionReportLine(
      size_t i, Function *f, size_t function_name_max_length,
      const Report::TimeUnits &units, const Report::OutputFormat &format) const;

  [[nodiscard]] static double convertTimeUnitsDouble(long long time,
                                                     double multiplier);

  [[nodiscard]] static size_t maxNumFunctionChars(
      const std::vector<Function *> &functions, size_t lowerLimit);

  [[nodiscard]] static std::string formatStringChar(size_t n);

  std::string m_sessionName;
  TimeUnits m_reportUnits;
  SortOrder m_sortOrder;
  SortType m_sortType;
};
}  // namespace SmartStack

#endif  // SMARTSTACK_LIBSMARTSTACK_REPORT_H_

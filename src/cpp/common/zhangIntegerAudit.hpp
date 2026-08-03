#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iomanip>
#include <limits>
#include <map>
#include <queue>
#include <sstream>
#include <set>
#include <string>
#include <utility>
#include <vector>
#include <boost/multiprecision/cpp_int.hpp>
#include "common/zhangFullRank.hpp"

using ZhangExactInteger = boost::multiprecision::cpp_int;
using ZhangExactVector  = std::vector<ZhangExactInteger>;
using ZhangExactMatrix  = std::vector<ZhangExactVector>;

using ZhangSatellitePair = std::pair<SatSys, SatSys>;

inline ZhangSatellitePair zhangCanonicalSatellitePair(
    SatSys left,
    SatSys right
)
{
    return right < left
        ? ZhangSatellitePair{right, left}
        : ZhangSatellitePair{left, right};
}

/** Redundancy of the satellite-only product graph induced by common receivers.
 *
 * Each receiver observing both satellites contributes one independent
 * length-two physical support path to that satellite relation.  The simple
 * bridge set describes topology, while edgeConnectivity uses the support
 * counts as integer capacities and therefore measures physical redundancy.
 */
struct ZhangSatelliteSupportMetrics
{
    std::set<SatSys>                         satellites;
    std::map<ZhangSatellitePair, int>        supportCounts;
    std::set<ZhangSatellitePair>             bridgeEdges;
    int                                      componentCount = 0;
    int                                      largestComponent = 0;
    int                                      edgeConnectivity = 0;
    int                                      minimumSupport = 0;
    int                                      maximumSupport = 0;
    double                                   meanSupport = 0;
};

inline int zhangIntegerCapacityMaxFlow(
    std::vector<std::vector<int>> capacity,
    std::size_t                   source,
    std::size_t                   sink
)
{
    const std::size_t size = capacity.size();
    int flow = 0;
    while (true)
    {
        std::vector<int> parent(size, -1);
        std::queue<std::size_t> pending;
        pending.push(source);
        parent[source] = static_cast<int>(source);
        while (!pending.empty() && parent[sink] < 0)
        {
            std::size_t from = pending.front();
            pending.pop();
            for (std::size_t to = 0; to < size; to++)
            {
                if (parent[to] < 0 && capacity[from][to] > 0)
                {
                    parent[to] = static_cast<int>(from);
                    pending.push(to);
                }
            }
        }
        if (parent[sink] < 0)
        {
            break;
        }
        int increment = std::numeric_limits<int>::max();
        for (std::size_t node = sink; node != source;)
        {
            std::size_t from = static_cast<std::size_t>(parent[node]);
            increment = std::min(increment, capacity[from][node]);
            node = from;
        }
        for (std::size_t node = sink; node != source;)
        {
            std::size_t from = static_cast<std::size_t>(parent[node]);
            capacity[from][node] -= increment;
            capacity[node][from] += increment;
            node = from;
        }
        flow += increment;
    }
    return flow;
}

inline ZhangSatelliteSupportMetrics zhangSatelliteSupportMetrics(
    const std::set<ZhangGraphEdge>& physicalEdges
)
{
    ZhangSatelliteSupportMetrics result;
    std::map<std::string, std::set<SatSys>> byReceiver;
    for (const auto& edge : physicalEdges)
    {
        result.satellites.insert(edge.satellite);
        byReceiver[edge.receiver].insert(edge.satellite);
    }
    for (const auto& [receiver, satellites] : byReceiver)
    {
        for (auto left = satellites.begin(); left != satellites.end(); left++)
        {
            auto right = left;
            for (++right; right != satellites.end(); right++)
            {
                result.supportCounts[{*left, *right}]++;
            }
        }
    }
    if (result.supportCounts.empty())
    {
        result.componentCount = result.satellites.size();
        result.largestComponent = result.satellites.empty() ? 0 : 1;
        return result;
    }

    long long supportSum = 0;
    result.minimumSupport = std::numeric_limits<int>::max();
    for (const auto& [pair, support] : result.supportCounts)
    {
        supportSum += support;
        result.minimumSupport = std::min(result.minimumSupport, support);
        result.maximumSupport = std::max(result.maximumSupport, support);
    }
    result.meanSupport = static_cast<double>(supportSum) /
                         result.supportCounts.size();

    std::vector<SatSys> satellites(
        result.satellites.begin(), result.satellites.end()
    );
    std::map<SatSys, std::size_t> index;
    for (std::size_t node = 0; node < satellites.size(); node++)
    {
        index[satellites[node]] = node;
    }
    std::vector<std::vector<std::size_t>> adjacency(satellites.size());
    std::vector<std::vector<int>> capacities(
        satellites.size(), std::vector<int>(satellites.size())
    );
    for (const auto& [pair, support] : result.supportCounts)
    {
        std::size_t left = index.at(pair.first);
        std::size_t right = index.at(pair.second);
        adjacency[left].push_back(right);
        adjacency[right].push_back(left);
        capacities[left][right] = support;
        capacities[right][left] = support;
    }

    std::vector<int> discovery(satellites.size(), -1);
    std::vector<int> low(satellites.size(), -1);
    int clock = 0;
    auto visit = [&](auto&& self, std::size_t node, std::size_t parent) -> void
    {
        discovery[node] = low[node] = clock++;
        for (std::size_t next : adjacency[node])
        {
            if (discovery[next] < 0)
            {
                self(self, next, node);
                low[node] = std::min(low[node], low[next]);
                if (low[next] > discovery[node])
                {
                    result.bridgeEdges.insert(
                        zhangCanonicalSatellitePair(
                            satellites[node], satellites[next]
                        )
                    );
                }
            }
            else if (next != parent)
            {
                low[node] = std::min(low[node], discovery[next]);
            }
        }
    };

    for (std::size_t start = 0; start < satellites.size(); start++)
    {
        if (discovery[start] >= 0)
        {
            continue;
        }
        result.componentCount++;
        std::queue<std::size_t> pending;
        std::set<std::size_t> component;
        pending.push(start);
        component.insert(start);
        while (!pending.empty())
        {
            std::size_t node = pending.front();
            pending.pop();
            for (std::size_t next : adjacency[node])
            {
                if (component.insert(next).second)
                {
                    pending.push(next);
                }
            }
        }
        result.largestComponent = std::max(
            result.largestComponent,
            static_cast<int>(component.size())
        );
        visit(visit, start, satellites.size());
    }

    if (result.componentCount == 1 && satellites.size() > 1)
    {
        result.edgeConnectivity = std::numeric_limits<int>::max();
        for (std::size_t target = 1; target < satellites.size(); target++)
        {
            result.edgeConnectivity = std::min(
                result.edgeConnectivity,
                zhangIntegerCapacityMaxFlow(capacities, 0, target)
            );
        }
    }
    return result;
}

/** Number of edge-disjoint alternative receiver-satellite paths after the
 * target physical edge is removed.  Zero means that edge is a bridge. */
inline int zhangAlternativePhysicalPathCount(
    const std::set<ZhangGraphEdge>& physicalEdges,
    const ZhangGraphEdge&           target
)
{
    std::set<std::string> nodes;
    auto receiverNode = [](const std::string& receiver)
    {
        return std::string("R:") + receiver;
    };
    auto satelliteNode = [](const SatSys& satellite)
    {
        return std::string("S:") +
               std::to_string(static_cast<int>(satellite.sys)) + ":" +
               std::to_string(satellite.prn);
    };
    for (const auto& edge : physicalEdges)
    {
        nodes.insert(receiverNode(edge.receiver));
        nodes.insert(satelliteNode(edge.satellite));
    }
    std::vector<std::string> ordered(nodes.begin(), nodes.end());
    std::map<std::string, std::size_t> index;
    for (std::size_t node = 0; node < ordered.size(); node++)
    {
        index[ordered[node]] = node;
    }
    auto source = index.find(receiverNode(target.receiver));
    auto sink = index.find(satelliteNode(target.satellite));
    if (source == index.end() || sink == index.end())
    {
        return 0;
    }
    std::vector<std::vector<int>> capacity(
        ordered.size(), std::vector<int>(ordered.size())
    );
    for (const auto& edge : physicalEdges)
    {
        if (edge == target)
        {
            continue;
        }
        std::size_t left = index.at(receiverNode(edge.receiver));
        std::size_t right = index.at(satelliteNode(edge.satellite));
        capacity[left][right] = 1;
        capacity[right][left] = 1;
    }
    return zhangIntegerCapacityMaxFlow(
        std::move(capacity), source->second, sink->second
    );
}

inline std::uint64_t zhangAuditFnv1a(std::uint64_t hash, const std::string& text)
{
    for (unsigned char character : text)
    {
        hash ^= character;
        hash *= 1099511628211ULL;
    }
    return hash;
}

inline std::string zhangExactMatrixFingerprint(const ZhangExactMatrix& matrix)
{
    std::uint64_t hash = 1469598103934665603ULL;
    for (const auto& row : matrix)
    {
        for (const auto& value : row)
        {
            hash = zhangAuditFnv1a(hash, value.convert_to<std::string>());
            hash = zhangAuditFnv1a(hash, ",");
        }
        hash = zhangAuditFnv1a(hash, ";");
    }
    std::ostringstream stream;
    stream << std::hex << std::setw(16) << std::setfill('0') << hash;
    return stream.str();
}

inline std::string zhangIntegerComponentId(const ZhangGraphBasis& basis)
{
    std::uint64_t hash = 1469598103934665603ULL;
    hash = zhangAuditFnv1a(hash, basis.rootReceiver);
    for (const auto& receiver : basis.receivers)
    {
        hash = zhangAuditFnv1a(hash, "R:" + receiver + ";");
    }
    for (const auto& satellite : basis.satellites)
    {
        hash = zhangAuditFnv1a(
            hash,
            "S:" + std::to_string(static_cast<int>(satellite.sys)) + ":" +
                std::to_string(satellite.prn) + ";"
        );
    }
    std::ostringstream stream;
    stream << basis.rootReceiver << "-" << std::hex << std::setw(16)
           << std::setfill('0') << hash;
    return stream.str();
}

inline std::string zhangAuditSatelliteLabel(const SatSys& satellite)
{
    return std::to_string(static_cast<int>(satellite.sys)) + ":" +
           std::to_string(satellite.prn);
}

inline ZhangExactMatrix zhangExactZeroMatrix(std::size_t rows, std::size_t columns)
{
    return ZhangExactMatrix(rows, ZhangExactVector(columns));
}

inline ZhangExactMatrix zhangExactIdentityMatrix(std::size_t size)
{
    ZhangExactMatrix identity = zhangExactZeroMatrix(size, size);
    for (std::size_t index = 0; index < size; index++)
    {
        identity[index][index] = 1;
    }
    return identity;
}

inline ZhangExactVector zhangExactRowTimesMatrix(
    const ZhangExactVector& row,
    const ZhangExactMatrix& matrix
)
{
    if (matrix.empty())
    {
        return {};
    }

    ZhangExactVector result(matrix.front().size());
    for (std::size_t inner = 0; inner < row.size(); inner++)
    {
        if (row[inner] == 0)
        {
            continue;
        }
        for (std::size_t column = 0; column < result.size(); column++)
        {
            result[column] += row[inner] * matrix[inner][column];
        }
    }
    return result;
}

inline ZhangExactMatrix zhangExactMultiply(
    const ZhangExactMatrix& left,
    const ZhangExactMatrix& right
)
{
    if (left.empty())
    {
        return {};
    }

    ZhangExactMatrix result = zhangExactZeroMatrix(left.size(), right.front().size());
    for (std::size_t row = 0; row < left.size(); row++)
    {
        result[row] = zhangExactRowTimesMatrix(left[row], right);
    }
    return result;
}

inline ZhangExactVector zhangExactMatrixTimesColumn(
    const ZhangExactMatrix& matrix,
    const ZhangExactVector& column
)
{
    ZhangExactVector result(matrix.size());
    for (std::size_t row = 0; row < matrix.size(); row++)
    {
        for (std::size_t index = 0; index < column.size(); index++)
        {
            result[row] += matrix[row][index] * column[index];
        }
    }
    return result;
}

inline ZhangExactInteger zhangExactDeterminant(ZhangExactMatrix matrix)
{
    const std::size_t size = matrix.size();
    if (size == 0)
    {
        return 1;
    }
    for (const auto& row : matrix)
    {
        if (row.size() != size)
        {
            return 0;
        }
    }

    ZhangExactInteger sign = 1;
    ZhangExactInteger previousPivot = 1;
    for (std::size_t pivot = 0; pivot + 1 < size; pivot++)
    {
        std::size_t selected = pivot;
        while (selected < size && matrix[selected][pivot] == 0)
        {
            selected++;
        }
        if (selected == size)
        {
            return 0;
        }
        if (selected != pivot)
        {
            std::swap(matrix[selected], matrix[pivot]);
            sign = -sign;
        }

        ZhangExactInteger pivotValue = matrix[pivot][pivot];
        for (std::size_t row = pivot + 1; row < size; row++)
        {
            for (std::size_t column = pivot + 1; column < size; column++)
            {
                matrix[row][column] =
                    (matrix[row][column] * pivotValue -
                     matrix[row][pivot] * matrix[pivot][column]) /
                    previousPivot;
            }
        }
        previousPivot = pivotValue;
    }
    return sign * matrix.back().back();
}

inline ZhangExactInteger zhangExactAbs(const ZhangExactInteger& value)
{
    return value < 0 ? -value : value;
}

struct ZhangIntegerLatticeMembership
{
    bool                          contained = false;
    int                           rank = 0;
    std::vector<ZhangExactInteger> smithInvariants;
    ZhangExactVector              combination;
};

struct ZhangExactRowHnf
{
    ZhangExactMatrix basis;
    ZhangExactVector values;
    bool              consistent = true;
};

inline ZhangExactInteger zhangExactFloorDivide(
    const ZhangExactInteger& numerator,
    const ZhangExactInteger& positiveDenominator
)
{
    ZhangExactInteger quotient = numerator / positiveDenominator;
    ZhangExactInteger remainder = numerator % positiveDenominator;
    if (remainder < 0)
    {
        quotient--;
    }
    return quotient;
}

/** Exact row-Hermite normalisation with the same unimodular row operations
 * applied to integer right-hand sides.
 *
 * Zero and integer-redundant rows are removed.  A zero coefficient row with a
 * non-zero transformed right-hand side marks an inconsistent constraint set.
 * This routine describes the exact lattice only; callers applying finite-noise
 * pseudo-observations must transform their noise covariance separately.
 */
inline ZhangExactRowHnf zhangExactRowHermiteNormalForm(
    ZhangExactMatrix rows,
    ZhangExactVector values = {}
)
{
    ZhangExactRowHnf result;
    if (rows.empty())
    {
        return result;
    }
    const std::size_t columns = rows.front().size();
    for (const auto& row : rows)
    {
        if (row.size() != columns)
        {
            result.consistent = false;
            return result;
        }
    }
    if (values.empty())
    {
        values.resize(rows.size());
    }
    if (values.size() != rows.size())
    {
        result.consistent = false;
        return result;
    }

    auto addRowMultiple = [&](std::size_t destination,
                              std::size_t source,
                              const ZhangExactInteger& multiplier)
    {
        for (std::size_t column = 0; column < columns; column++)
        {
            rows[destination][column] += multiplier * rows[source][column];
        }
        values[destination] += multiplier * values[source];
    };
    auto negateRow = [&](std::size_t row)
    {
        for (auto& value : rows[row])
        {
            value = -value;
        }
        values[row] = -values[row];
    };

    std::size_t pivotRow = 0;
    for (std::size_t column = 0;
         column < columns && pivotRow < rows.size();
         column++)
    {
        std::size_t selected = rows.size();
        for (std::size_t row = pivotRow; row < rows.size(); row++)
        {
            if (rows[row][column] != 0 &&
                (selected == rows.size() ||
                 zhangExactAbs(rows[row][column]) <
                     zhangExactAbs(rows[selected][column])))
            {
                selected = row;
            }
        }
        if (selected == rows.size())
        {
            continue;
        }
        std::swap(rows[pivotRow], rows[selected]);
        std::swap(values[pivotRow], values[selected]);

        bool reduced = false;
        do
        {
            reduced = true;
            for (std::size_t row = pivotRow + 1; row < rows.size(); row++)
            {
                if (rows[row][column] == 0)
                {
                    continue;
                }
                ZhangExactInteger quotient =
                    rows[row][column] / rows[pivotRow][column];
                addRowMultiple(row, pivotRow, -quotient);
                if (rows[row][column] != 0)
                {
                    std::swap(rows[row], rows[pivotRow]);
                    std::swap(values[row], values[pivotRow]);
                }
                reduced = false;
                break;
            }
        }
        while (!reduced);

        if (rows[pivotRow][column] < 0)
        {
            negateRow(pivotRow);
        }
        for (std::size_t row = 0; row < pivotRow; row++)
        {
            if (rows[row][column] == 0)
            {
                continue;
            }
            ZhangExactInteger quotient = zhangExactFloorDivide(
                rows[row][column],
                rows[pivotRow][column]
            );
            addRowMultiple(row, pivotRow, -quotient);
        }
        pivotRow++;
    }

    for (std::size_t row = 0; row < rows.size(); row++)
    {
        bool nonzero = std::any_of(
            rows[row].begin(),
            rows[row].end(),
            [](const auto& value) { return value != 0; }
        );
        if (!nonzero)
        {
            result.consistent &= values[row] == 0;
            continue;
        }
        result.basis.push_back(std::move(rows[row]));
        result.values.push_back(std::move(values[row]));
    }
    return result;
}

/** Saturated integer kernel of matrix: every returned row u satisfies A u=0.
 *
 * Exact unimodular column operations produce A V = D after arbitrary exact
 * row operations.  Columns of V beyond rank(D) form a Z-basis of the kernel,
 * not merely a rational nullspace with rounded coefficients.
 */
inline ZhangExactMatrix zhangExactIntegerKernel(
    ZhangExactMatrix matrix,
    std::size_t      emptyColumnCount = 0
)
{
    const std::size_t rowCount = matrix.size();
    const std::size_t columnCount = matrix.empty()
        ? emptyColumnCount
        : matrix.front().size();
    for (const auto& row : matrix)
    {
        if (row.size() != columnCount)
        {
            return {};
        }
    }
    if (rowCount == 0)
    {
        ZhangExactMatrix kernel;
        for (std::size_t column = 0; column < columnCount; column++)
        {
            ZhangExactVector unit(columnCount);
            unit[column] = 1;
            kernel.push_back(std::move(unit));
        }
        return kernel;
    }

    ZhangExactMatrix rightTransform = zhangExactIdentityMatrix(columnCount);
    auto swapRows = [&](std::size_t left, std::size_t right)
    {
        std::swap(matrix[left], matrix[right]);
    };
    auto addRowMultiple = [&](std::size_t destination,
                              std::size_t source,
                              const ZhangExactInteger& multiplier)
    {
        for (std::size_t column = 0; column < columnCount; column++)
        {
            matrix[destination][column] += multiplier * matrix[source][column];
        }
    };
    auto swapColumns = [&](std::size_t left, std::size_t right)
    {
        for (auto& row : matrix)
        {
            std::swap(row[left], row[right]);
        }
        for (auto& row : rightTransform)
        {
            std::swap(row[left], row[right]);
        }
    };
    auto addColumnMultiple = [&](std::size_t destination,
                                 std::size_t source,
                                 const ZhangExactInteger& multiplier)
    {
        for (auto& row : matrix)
        {
            row[destination] += multiplier * row[source];
        }
        for (auto& row : rightTransform)
        {
            row[destination] += multiplier * row[source];
        }
    };

    std::size_t rank = 0;
    while (rank < rowCount && rank < columnCount)
    {
        std::size_t selectedRow = rowCount;
        std::size_t selectedColumn = columnCount;
        for (std::size_t row = rank; row < rowCount; row++)
        {
            for (std::size_t column = rank; column < columnCount; column++)
            {
                if (matrix[row][column] != 0 &&
                    (selectedRow == rowCount ||
                     zhangExactAbs(matrix[row][column]) <
                         zhangExactAbs(matrix[selectedRow][selectedColumn])))
                {
                    selectedRow = row;
                    selectedColumn = column;
                }
            }
        }
        if (selectedRow == rowCount)
        {
            break;
        }
        swapRows(rank, selectedRow);
        swapColumns(rank, selectedColumn);

        while (true)
        {
            bool restart = false;
            for (std::size_t row = rank + 1; row < rowCount; row++)
            {
                if (matrix[row][rank] == 0)
                {
                    continue;
                }
                ZhangExactInteger quotient =
                    matrix[row][rank] / matrix[rank][rank];
                addRowMultiple(row, rank, -quotient);
                if (matrix[row][rank] != 0)
                {
                    swapRows(row, rank);
                }
                restart = true;
                break;
            }
            if (restart)
            {
                continue;
            }
            for (std::size_t column = rank + 1;
                 column < columnCount;
                 column++)
            {
                if (matrix[rank][column] == 0)
                {
                    continue;
                }
                ZhangExactInteger quotient =
                    matrix[rank][column] / matrix[rank][rank];
                addColumnMultiple(column, rank, -quotient);
                if (matrix[rank][column] != 0)
                {
                    swapColumns(column, rank);
                }
                restart = true;
                break;
            }
            if (!restart)
            {
                break;
            }
        }
        rank++;
    }

    ZhangExactMatrix kernel;
    for (std::size_t column = rank; column < columnCount; column++)
    {
        ZhangExactVector vector(columnCount);
        for (std::size_t row = 0; row < columnCount; row++)
        {
            vector[row] = rightTransform[row][column];
        }
        kernel.push_back(std::move(vector));
    }
    return kernel;
}

struct ZhangExactSurvivingLattice
{
    ZhangExactMatrix basis;
    ZhangExactVector values;
    bool              consistent = true;
    int               touchedRows = 0;
    int               combinationRank = 0;
};

/** Exact intersection of a held row lattice with the surviving columns.
 *
 * For H=[H_S H_R], rows of the integer kernel of H_R^T are precisely the
 * integer row combinations that eliminate every removed coordinate.  The
 * same combinations are applied to H_S and the integer right-hand side,
 * followed by exact row-HNF normalisation.
 */
inline ZhangExactSurvivingLattice zhangExactSurvivingLattice(
    const ZhangExactMatrix& rows,
    const ZhangExactVector& values,
    const std::vector<bool>& survivingColumns
)
{
    ZhangExactSurvivingLattice result;
    if (rows.size() != values.size())
    {
        result.consistent = false;
        return result;
    }
    for (const auto& row : rows)
    {
        if (row.size() != survivingColumns.size())
        {
            result.consistent = false;
            return result;
        }
    }

    std::vector<std::size_t> surviving;
    std::vector<std::size_t> removed;
    for (std::size_t column = 0; column < survivingColumns.size(); column++)
    {
        (survivingColumns[column] ? surviving : removed).push_back(column);
    }
    for (const auto& row : rows)
    {
        result.touchedRows += std::any_of(
            removed.begin(),
            removed.end(),
            [&](std::size_t column) { return row[column] != 0; }
        );
    }

    if (removed.empty())
    {
        result.combinationRank = rows.size();
        ZhangExactRowHnf hnf = zhangExactRowHermiteNormalForm(rows, values);
        result.basis = std::move(hnf.basis);
        result.values = std::move(hnf.values);
        result.consistent = hnf.consistent;
        return result;
    }

    // H_R^T has one row per removed coordinate and one column per held row.
    ZhangExactMatrix removedTranspose = zhangExactZeroMatrix(
        removed.size(), rows.size()
    );
    for (std::size_t removedRow = 0; removedRow < removed.size(); removedRow++)
    {
        for (std::size_t heldRow = 0; heldRow < rows.size(); heldRow++)
        {
            removedTranspose[removedRow][heldRow] =
                rows[heldRow][removed[removedRow]];
        }
    }
    ZhangExactMatrix combinations = zhangExactIntegerKernel(
        std::move(removedTranspose),
        rows.size()
    );
    result.combinationRank = combinations.size();

    ZhangExactMatrix survivingRows;
    ZhangExactVector survivingValues;
    for (const auto& combination : combinations)
    {
        ZhangExactVector row(surviving.size());
        ZhangExactInteger value = 0;
        for (std::size_t heldRow = 0; heldRow < rows.size(); heldRow++)
        {
            value += combination[heldRow] * values[heldRow];
            for (std::size_t column = 0; column < surviving.size(); column++)
            {
                row[column] +=
                    combination[heldRow] * rows[heldRow][surviving[column]];
            }
        }
        survivingRows.push_back(std::move(row));
        survivingValues.push_back(std::move(value));
    }
    ZhangExactRowHnf hnf = zhangExactRowHermiteNormalForm(
        std::move(survivingRows),
        std::move(survivingValues)
    );
    result.basis = std::move(hnf.basis);
    result.values = std::move(hnf.values);
    result.consistent = hnf.consistent;
    return result;
}

/** Exact membership in the row lattice generated by rows.
 *
 * The implementation diagonalises rows^T using unimodular Euclidean row and
 * column operations.  Applying the same row operations to target gives the
 * exact divisibility test for A x = target.  No floating-point rank or rounded
 * inverse is used.
 */
inline ZhangIntegerLatticeMembership zhangIntegerRowLatticeContains(
    const ZhangExactMatrix& rows,
    const ZhangExactVector& target
)
{
    ZhangIntegerLatticeMembership result;
    if (target.empty())
    {
        result.contained = true;
        return result;
    }

    const std::size_t rowCount = rows.size();
    const std::size_t dimension = target.size();
    for (const auto& row : rows)
    {
        if (row.size() != dimension)
        {
            return result;
        }
    }

    // A x = target, where A is rows transposed.
    ZhangExactMatrix matrix = zhangExactZeroMatrix(dimension, rowCount);
    for (std::size_t row = 0; row < rowCount; row++)
    {
        for (std::size_t column = 0; column < dimension; column++)
        {
            matrix[column][row] = rows[row][column];
        }
    }
    ZhangExactVector transformed = target;
    ZhangExactMatrix rightTransform = zhangExactIdentityMatrix(rowCount);

    auto swapRows = [&](std::size_t left, std::size_t right)
    {
        std::swap(matrix[left], matrix[right]);
        std::swap(transformed[left], transformed[right]);
    };
    auto addRowMultiple = [&](std::size_t destination,
                              std::size_t source,
                              const ZhangExactInteger& multiplier)
    {
        for (std::size_t column = 0; column < rowCount; column++)
        {
            matrix[destination][column] += multiplier * matrix[source][column];
        }
        transformed[destination] += multiplier * transformed[source];
    };
    auto swapColumns = [&](std::size_t left, std::size_t right)
    {
        for (auto& row : matrix)
        {
            std::swap(row[left], row[right]);
        }
        for (auto& row : rightTransform)
        {
            std::swap(row[left], row[right]);
        }
    };
    auto addColumnMultiple = [&](std::size_t destination,
                                 std::size_t source,
                                 const ZhangExactInteger& multiplier)
    {
        for (auto& row : matrix)
        {
            row[destination] += multiplier * row[source];
        }
        for (auto& row : rightTransform)
        {
            row[destination] += multiplier * row[source];
        }
    };

    std::size_t pivot = 0;
    while (pivot < dimension && pivot < rowCount)
    {
        std::size_t selectedRow = dimension;
        std::size_t selectedColumn = rowCount;
        for (std::size_t row = pivot; row < dimension; row++)
        {
            for (std::size_t column = pivot; column < rowCount; column++)
            {
                if (matrix[row][column] != 0 &&
                    (selectedRow == dimension ||
                     zhangExactAbs(matrix[row][column]) <
                         zhangExactAbs(matrix[selectedRow][selectedColumn])))
                {
                    selectedRow = row;
                    selectedColumn = column;
                }
            }
        }
        if (selectedRow == dimension)
        {
            break;
        }
        swapRows(pivot, selectedRow);
        swapColumns(pivot, selectedColumn);

        while (true)
        {
            bool restart = false;
            for (std::size_t row = pivot + 1; row < dimension; row++)
            {
                if (matrix[row][pivot] == 0)
                {
                    continue;
                }
                ZhangExactInteger quotient = matrix[row][pivot] / matrix[pivot][pivot];
                addRowMultiple(row, pivot, -quotient);
                if (matrix[row][pivot] != 0)
                {
                    swapRows(row, pivot);
                }
                restart = true;
                break;
            }
            if (restart)
            {
                continue;
            }

            for (std::size_t column = pivot + 1; column < rowCount; column++)
            {
                if (matrix[pivot][column] == 0)
                {
                    continue;
                }
                ZhangExactInteger quotient =
                    matrix[pivot][column] / matrix[pivot][pivot];
                addColumnMultiple(column, pivot, -quotient);
                if (matrix[pivot][column] != 0)
                {
                    swapColumns(column, pivot);
                }
                restart = true;
                break;
            }
            if (restart)
            {
                continue;
            }

            std::size_t offendingRow = dimension;
            for (std::size_t row = pivot + 1; row < dimension; row++)
            {
                for (std::size_t column = pivot + 1; column < rowCount; column++)
                {
                    if (matrix[row][column] % matrix[pivot][pivot] != 0)
                    {
                        offendingRow = row;
                        break;
                    }
                }
                if (offendingRow != dimension)
                {
                    break;
                }
            }
            if (offendingRow != dimension)
            {
                addRowMultiple(pivot, offendingRow, 1);
                continue;
            }
            break;
        }

        if (matrix[pivot][pivot] < 0)
        {
            addRowMultiple(pivot, pivot, -2);
        }
        result.smithInvariants.push_back(matrix[pivot][pivot]);
        pivot++;
    }

    result.rank = static_cast<int>(pivot);
    result.contained = true;
    for (std::size_t row = 0; row < pivot; row++)
    {
        if (transformed[row] % matrix[row][row] != 0)
        {
            result.contained = false;
        }
    }
    for (std::size_t row = pivot; row < dimension; row++)
    {
        if (transformed[row] != 0)
        {
            result.contained = false;
        }
    }
    if (result.contained)
    {
        ZhangExactVector diagonalCoordinates(rowCount);
        for (std::size_t row = 0; row < pivot; row++)
        {
            diagonalCoordinates[row] = transformed[row] / matrix[row][row];
        }
        result.combination = zhangExactMatrixTimesColumn(
            rightTransform,
            diagonalCoordinates
        );
    }
    return result;
}

struct ZhangDualSignalLatticeValidity
{
    bool l1 = false;
    bool l2 = false;
    bool wideLane = false;
};

inline ZhangDualSignalLatticeValidity zhangClassifyDualSignalLattice(
    const ZhangExactMatrix& heldRows,
    std::size_t              signalDimension
)
{
    ZhangDualSignalLatticeValidity validity{true, true, true};
    for (std::size_t index = 0; index < signalDimension; index++)
    {
        ZhangExactVector l1(2 * signalDimension);
        ZhangExactVector l2(2 * signalDimension);
        ZhangExactVector wideLane(2 * signalDimension);
        l1[index] = 1;
        l2[signalDimension + index] = 1;
        wideLane[index] = 1;
        wideLane[signalDimension + index] = -1;

        validity.l1 &= zhangIntegerRowLatticeContains(heldRows, l1).contained;
        validity.l2 &= zhangIntegerRowLatticeContains(heldRows, l2).contained;
        validity.wideLane &=
            zhangIntegerRowLatticeContains(heldRows, wideLane).contained;
    }
    return validity;
}

struct ZhangCanonicalIntegerAudit
{
    std::vector<ZhangGraphEdge> treeEdges;
    std::vector<ZhangGraphEdge> chordEdges;
    std::vector<std::string>    coordinateNodes;
    ZhangExactMatrix            treeInverse;
    ZhangExactMatrix            chordTreeMap;
    ZhangExactMatrix            canonicalToArc;
    ZhangExactMatrix            satelliteDatumSingleDifferences;
    ZhangExactMatrix            satelliteFixQuotient;
    std::string                 canonicalToArcFingerprint;
    std::string                 datumMappingFingerprint;
    std::string                 fixQuotientFingerprint;
    std::string                 failureReason;
    bool                        denseCanonicalMaterialised = false;
    bool                        valid = false;
};

inline ZhangCanonicalIntegerAudit zhangCanonicalIntegerAudit(
    const ZhangGraphBasis& basis
)
{
    using namespace zhang_graph_detail;

    ZhangCanonicalIntegerAudit audit;
    if (!basis.connected)
    {
        audit.failureReason = "basis_not_connected";
        return audit;
    }

    audit.treeEdges.assign(basis.treeEdges.begin(), basis.treeEdges.end());
    std::set_difference(
        basis.edges.begin(),
        basis.edges.end(),
        basis.treeEdges.begin(),
        basis.treeEdges.end(),
        std::back_inserter(audit.chordEdges)
    );

    const std::size_t treeSize = audit.treeEdges.size();
    std::map<std::string, ZhangExactVector> nodeExpression;
    nodeExpression[receiverNode(basis.rootReceiver)] = ZhangExactVector(treeSize);

    bool progress = true;
    while (progress)
    {
        progress = false;
        for (std::size_t edgeIndex = 0; edgeIndex < treeSize; edgeIndex++)
        {
            const auto& edge = audit.treeEdges[edgeIndex];
            std::string receiver = receiverNode(edge.receiver);
            std::string satellite = satelliteNode(edge.satellite);
            bool receiverKnown = nodeExpression.find(receiver) != nodeExpression.end();
            bool satelliteKnown = nodeExpression.find(satellite) != nodeExpression.end();
            if (receiverKnown == satelliteKnown)
            {
                continue;
            }

            const std::string& known = receiverKnown ? receiver : satellite;
            const std::string& unknown = receiverKnown ? satellite : receiver;
            ZhangExactVector expression = nodeExpression.at(known);
            for (auto& coefficient : expression)
            {
                coefficient = -coefficient;
            }
            expression[edgeIndex] += 1;
            nodeExpression[unknown] = std::move(expression);
            progress = true;
        }
    }

    if (nodeExpression.size() != basis.receivers.size() + basis.satellites.size())
    {
        audit.failureReason = "tree_does_not_span_declared_nodes";
        return audit;
    }

    for (const auto& receiver : basis.receivers)
    {
        if (receiver != basis.rootReceiver)
        {
            audit.coordinateNodes.push_back(receiverNode(receiver));
        }
    }
    for (const auto& satellite : basis.satellites)
    {
        audit.coordinateNodes.push_back(satelliteNode(satellite));
    }
    for (const auto& node : audit.coordinateNodes)
    {
        auto expression = nodeExpression.find(node);
        if (expression == nodeExpression.end())
        {
            audit.failureReason = "missing_tree_node:" + node;
            return audit;
        }
        audit.treeInverse.push_back(expression->second);
    }

    audit.chordTreeMap = zhangExactZeroMatrix(audit.chordEdges.size(), treeSize);
    for (std::size_t chord = 0; chord < audit.chordEdges.size(); chord++)
    {
        const auto& edge = audit.chordEdges[chord];
        auto receiver = nodeExpression.find(receiverNode(edge.receiver));
        auto satellite = nodeExpression.find(satelliteNode(edge.satellite));
        if (receiver == nodeExpression.end() || satellite == nodeExpression.end())
        {
            audit.failureReason =
                "missing_chord_endpoint:" + edge.receiver + ":" +
                zhangAuditSatelliteLabel(edge.satellite);
            return audit;
        }
        for (std::size_t column = 0; column < treeSize; column++)
        {
            audit.chordTreeMap[chord][column] =
                receiver->second[column] + satellite->second[column];
        }
    }

    const std::size_t arcCount = treeSize + audit.chordEdges.size();
    // The global graph has thousands of arcs, while W=[I 0; B I] is sparse by
    // construction.  Materialising an arcCount^2 cpp_int matrix would consume
    // gigabytes solely for diagnostics.  Keep dense W only for exact small-
    // graph tests and fingerprint the same integer structure sparsely in
    // production.
    constexpr std::size_t denseAuditArcLimit = 256;
    if (arcCount <= denseAuditArcLimit)
    {
        audit.canonicalToArc = zhangExactZeroMatrix(arcCount, arcCount);
        for (std::size_t edge = 0; edge < treeSize; edge++)
        {
            audit.canonicalToArc[edge][edge] = 1;
        }
        for (std::size_t chord = 0; chord < audit.chordEdges.size(); chord++)
        {
            for (std::size_t tree = 0; tree < treeSize; tree++)
            {
                audit.canonicalToArc[treeSize + chord][tree] =
                    audit.chordTreeMap[chord][tree];
            }
            audit.canonicalToArc[treeSize + chord][treeSize + chord] = 1;
        }
        audit.denseCanonicalMaterialised = true;
        audit.canonicalToArcFingerprint =
            zhangExactMatrixFingerprint(audit.canonicalToArc);
    }
    else
    {
        std::uint64_t hash = 1469598103934665603ULL;
        hash = zhangAuditFnv1a(
            hash,
            "rows=" + std::to_string(arcCount) +
                ";cols=" + std::to_string(arcCount) + ";"
        );
        for (std::size_t edge = 0; edge < treeSize; edge++)
        {
            hash = zhangAuditFnv1a(
                hash,
                std::to_string(edge) + ":" + std::to_string(edge) + ":1;"
            );
        }
        for (std::size_t chord = 0; chord < audit.chordEdges.size(); chord++)
        {
            for (std::size_t tree = 0; tree < treeSize; tree++)
            {
                const auto& coefficient = audit.chordTreeMap[chord][tree];
                if (coefficient == 0)
                {
                    continue;
                }
                hash = zhangAuditFnv1a(
                    hash,
                    std::to_string(treeSize + chord) + ":" +
                        std::to_string(tree) + ":" +
                        coefficient.convert_to<std::string>() + ";"
                );
            }
            hash = zhangAuditFnv1a(
                hash,
                std::to_string(treeSize + chord) + ":" +
                    std::to_string(treeSize + chord) + ":1;"
            );
        }
        std::ostringstream stream;
        stream << std::hex << std::setw(16) << std::setfill('0') << hash;
        audit.canonicalToArcFingerprint = stream.str();
    }

    if (basis.satellites.size() > 1)
    {
        const SatSys& referenceSatellite = *basis.satellites.begin();
        auto reference = nodeExpression.find(satelliteNode(referenceSatellite));
        if (reference == nodeExpression.end())
        {
            audit.failureReason =
                "missing_reference_satellite:" +
                zhangAuditSatelliteLabel(referenceSatellite);
            return audit;
        }
        for (const auto& satellite : basis.satellites)
        {
            if (satellite == referenceSatellite)
            {
                continue;
            }
            auto expression = nodeExpression.find(satelliteNode(satellite));
            if (expression == nodeExpression.end())
            {
                audit.failureReason =
                    "missing_satellite_node:" +
                    zhangAuditSatelliteLabel(satellite);
                return audit;
            }
            ZhangExactVector difference = expression->second;
            for (std::size_t column = 0; column < treeSize; column++)
            {
                difference[column] -= reference->second[column];
            }
            audit.satelliteDatumSingleDifferences.push_back(std::move(difference));
        }
    }
    audit.satelliteFixQuotient = zhangExactZeroMatrix(
        audit.satelliteDatumSingleDifferences.size(),
        audit.chordEdges.size()
    );
    audit.datumMappingFingerprint =
        zhangExactMatrixFingerprint(audit.satelliteDatumSingleDifferences);
    audit.fixQuotientFingerprint =
        zhangExactMatrixFingerprint(audit.satelliteFixQuotient);
    audit.valid = true;
    return audit;
}

struct ZhangSatelliteProductTarget
{
    ZhangExactMatrix             matrix;
    std::vector<ZhangGraphEdge>  currentChords;
    std::vector<SatSys>          targetSatellites;
    SatSys                       referenceSatellite;
    bool                         valid = false;
    std::string                  failureReason;
};

/** Exact G_sat for converting a current dynamic-tree ambiguity datum to a
 * persistent product-tree datum.
 *
 * This implements D_S P_S A_TP^{-1} S_TP E_C.  The current cycle coordinates
 * k are inserted only on current chord edges; a product-tree edge therefore
 * contributes a unit column exactly when it is a current chord.  The inverse
 * product-tree incidence is already available as the exact treeInverse map.
 */
inline ZhangSatelliteProductTarget zhangBuildSatelliteProductTarget(
    const ZhangGraphBasis& currentBasis,
    const ZhangGraphBasis& productBasis,
    const SatSys&          requestedReference = SatSys()
)
{
    ZhangSatelliteProductTarget result;
    if (currentBasis.receivers != productBasis.receivers ||
        currentBasis.satellites != productBasis.satellites ||
        currentBasis.rootReceiver != productBasis.rootReceiver)
    {
        result.failureReason = "product_tree_node_or_root_mismatch";
        return result;
    }
    if (!std::includes(
            currentBasis.edges.begin(), currentBasis.edges.end(),
            productBasis.treeEdges.begin(), productBasis.treeEdges.end()
        ))
    {
        result.failureReason = "product_tree_edge_not_in_current_graph";
        return result;
    }

    ZhangCanonicalIntegerAudit current =
        zhangCanonicalIntegerAudit(currentBasis);
    ZhangCanonicalIntegerAudit product =
        zhangCanonicalIntegerAudit(productBasis);
    if (!current.valid || !product.valid)
    {
        result.failureReason = !current.valid
            ? "current_graph_invalid:" + current.failureReason
            : "product_graph_invalid:" + product.failureReason;
        return result;
    }
    result.currentChords = current.chordEdges;
    if (productBasis.satellites.empty())
    {
        result.failureReason = "product_tree_has_no_satellite";
        return result;
    }
    result.referenceSatellite = requestedReference;
    if (result.referenceSatellite.prn == 0)
    {
        result.referenceSatellite = *productBasis.satellites.begin();
    }
    if (productBasis.satellites.find(result.referenceSatellite) ==
        productBasis.satellites.end())
    {
        result.failureReason = "reference_satellite_not_in_component";
        return result;
    }

    map<ZhangGraphEdge, std::size_t> productTreeIndex;
    for (std::size_t edge = 0; edge < product.treeEdges.size(); edge++)
    {
        productTreeIndex[product.treeEdges[edge]] = edge;
    }
    ZhangExactMatrix treeSelection = zhangExactZeroMatrix(
        product.treeEdges.size(), current.chordEdges.size()
    );
    for (std::size_t chord = 0; chord < current.chordEdges.size(); chord++)
    {
        auto treeEdge = productTreeIndex.find(current.chordEdges[chord]);
        if (treeEdge != productTreeIndex.end())
        {
            treeSelection[treeEdge->second][chord] = 1;
        }
    }
    ZhangExactMatrix nodeOffsets = zhangExactMultiply(
        product.treeInverse, treeSelection
    );

    const std::size_t satelliteOffset =
        productBasis.receivers.size() - 1;
    map<SatSys, std::size_t> satelliteRow;
    std::size_t row = satelliteOffset;
    for (const auto& satellite : productBasis.satellites)
    {
        satelliteRow[satellite] = row++;
    }
    const auto& reference = nodeOffsets.at(
        satelliteRow.at(result.referenceSatellite)
    );
    for (const auto& satellite : productBasis.satellites)
    {
        if (satellite == result.referenceSatellite)
        {
            continue;
        }
        ZhangExactVector target = nodeOffsets.at(satelliteRow.at(satellite));
        for (std::size_t column = 0; column < target.size(); column++)
        {
            target[column] -= reference[column];
        }
        result.targetSatellites.push_back(satellite);
        result.matrix.push_back(std::move(target));
    }
    result.valid = true;
    return result;
}

/** Named builder kept separate from the dynamic graph controller so product
 * target construction has no ownership over the estimation S-basis. */
struct ZhangProductTargetBuilder
{
    static ZhangSatelliteProductTarget build(
        const ZhangGraphBasis& currentBasis,
        const ZhangGraphBasis& productBasis,
        const SatSys& requestedReference = SatSys()
    )
    {
        return zhangBuildSatelliteProductTarget(
            currentBasis, productBasis, requestedReference
        );
    }
};

/** Recover individually proven named product targets from arbitrary fixed
 * integer combinations returned by LAMBDA/PAR.  Only exact integer row-lattice
 * membership is accepted; real-valued rank is deliberately insufficient. */
struct ProductConstraintPromotion
{
    static std::map<std::size_t, ZhangExactInteger> recoverNamedTargets(
        const ZhangExactMatrix& fixedRows,
        const ZhangExactVector& fixedValues,
        std::size_t             namedTargetCount
    )
    {
        std::map<std::size_t, ZhangExactInteger> recovered;
        if (fixedRows.size() != fixedValues.size())
        {
            return recovered;
        }
        for (std::size_t target = 0; target < namedTargetCount; target++)
        {
            ZhangExactVector unit(namedTargetCount);
            unit[target] = 1;
            ZhangIntegerLatticeMembership membership =
                zhangIntegerRowLatticeContains(fixedRows, unit);
            if (!membership.contained ||
                membership.combination.size() != fixedValues.size())
            {
                continue;
            }
            ZhangExactInteger value = 0;
            for (std::size_t row = 0; row < fixedValues.size(); row++)
            {
                value += membership.combination[row] * fixedValues[row];
            }
            recovered[target] = value;
        }
        return recovered;
    }
};

inline ZhangExactMatrix zhangCanonicalTransition(
    const ZhangGraphBasis& oldBasis,
    const ZhangGraphBasis& newBasis
)
{
    ZhangCanonicalIntegerAudit oldAudit = zhangCanonicalIntegerAudit(oldBasis);
    ZhangCanonicalIntegerAudit newAudit = zhangCanonicalIntegerAudit(newBasis);
    if (!oldAudit.valid || !newAudit.valid ||
        !oldAudit.denseCanonicalMaterialised ||
        !newAudit.denseCanonicalMaterialised ||
        oldBasis.edges != newBasis.edges)
    {
        return {};
    }

    std::vector<ZhangGraphEdge> oldArcs = oldAudit.treeEdges;
    oldArcs.insert(oldArcs.end(), oldAudit.chordEdges.begin(), oldAudit.chordEdges.end());
    std::map<ZhangGraphEdge, std::size_t> oldArcIndex;
    for (std::size_t index = 0; index < oldArcs.size(); index++)
    {
        oldArcIndex[oldArcs[index]] = index;
    }

    ZhangExactMatrix transition;
    for (const auto& edge : newAudit.treeEdges)
    {
        transition.push_back(oldAudit.canonicalToArc.at(oldArcIndex.at(edge)));
    }
    for (const auto& chord : newAudit.chordEdges)
    {
        ZhangExactVector rawCycle(oldArcs.size());
        for (const auto& [edge, coefficient] : zhangFundamentalCycle(newBasis, chord))
        {
            rawCycle[oldArcIndex.at(edge)] = coefficient;
        }
        transition.push_back(
            zhangExactRowTimesMatrix(rawCycle, oldAudit.canonicalToArc)
        );
    }
    return transition;
}

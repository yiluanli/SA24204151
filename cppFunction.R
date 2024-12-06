library(Rcpp)
cppFunction('
List calc_complex_stats(NumericVector x) {
    int n = x.size();
    
    // 初始化统计量
    double mean_x = 0;
    double variance = 0;
    double std_dev = 0;
    double max_val = R_NegInf;  // 初始化为负无穷，方便找到最大值
    double min_val = R_PosInf;  // 初始化为正无穷，方便找到最小值
    
    int count_non_na = 0;  // 非 NA 的元素数量
    std::vector<double> non_na_values;
    
    // 计算均值、方差、最大值、最小值
    for(int i = 0; i < n; ++i) {
        if (R_IsNA(x[i])) {
            continue;  // 如果是 NA，跳过
        }
        
        double value = x[i];
        mean_x += value;
        variance += value * value;
        max_val = std::max(max_val, value);
        min_val = std::min(min_val, value);
        
        non_na_values.push_back(value);  // 保存非 NA 值
        
        count_non_na++;
    }
    
    // 计算均值
    if(count_non_na > 0) {
        mean_x /= count_non_na;
    }
    
    // 计算方差
    if(count_non_na > 1) {
        variance = (variance / count_non_na) - (mean_x * mean_x);
    }
    
    // 计算标准差
    std_dev = sqrt(variance);
    
    // 计算中位数
    std::sort(non_na_values.begin(), non_na_values.end());
    double median_x;
    int non_na_size = non_na_values.size();
    if(non_na_size % 2 == 0) {
        median_x = (non_na_values[non_na_size / 2 - 1] + non_na_values[non_na_size / 2]) / 2.0;
    } else {
        median_x = non_na_values[non_na_size / 2];
    }
    
    // 返回统计结果
    return List::create(Named("mean") = mean_x,
                        Named("variance") = variance,
                        Named("std_dev") = std_dev,
                        Named("min") = min_val,
                        Named("max") = max_val,
                        Named("median") = median_x);
}
')


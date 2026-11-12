IARC_FollowUpRule <- data.frame(
  group = make_group(5),
  rule = c(
    "诊断依据编码为 0, 发病日期应该等于死亡日期",
    "生存状态编码为 2, 死亡日期不能缺失",
    "生存状态编码不为 2, 死亡日期应该为空",
    "生存状态编码为 2, 随访截止日期应该等于死亡日期",
    "死亡日期应该在发病日期之后"
  ),
  stringsAsFactors = FALSE
)
